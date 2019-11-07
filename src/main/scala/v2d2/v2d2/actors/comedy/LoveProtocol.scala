package v2d2.actions.love

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import fastparse._, SingleLineWhitespace._
import spray.json.DefaultJsonProtocol
import slack.models.Message
import slack.models.User
import v2d2.actors.core.BotCombinators

sealed trait EmailUser {
  def user: User

  def userEmail = {
    email(user).getOrElse("noreply@rallyhealth.com")
  }

  protected def email(
    u: User
  ): Option[String] = {
    u.profile match {
      case Some(p) =>
        p.email
      case _ => None
    }
  }
}

case class GuessLove(
  message: Message,
  count: Int
)

case class SendUsersLove(
  user: User,
  recipients: Seq[User],
  message: Option[String],
  msg: Message,
  volume: Int = 0 // 0 = guessing game, 1 = tell them quietly, 2 = tell the channel
) extends EmailUser {

  def lovable = {
    (email(user) != None) && (targetsEmails.length > 0)
  }

  def targetsEmails: Seq[String] = {
    recipients
      .filter { u =>
        u.profile match {
          case Some(p) =>
            p.email match {
              case Some(e) => true
              case _       => false
            }
          case _ => false
        }
      }
      .map { u =>
        u.profile match {
          case Some(p) =>
            p.email.getOrElse("noreply@rallyhealth.com")
          case _ => "noreply@rallyhealth.com"
        }
      }
  }
}

case class GetUsersSentLove(
  user: User,
  senderNick: String,
  msg: Message
) extends EmailUser {

  def lovable = {
    userEmail != "noreply@rallyhealth.com"
  }
}

case class GetUsersLove(
  user: User,
  senderNick: String,
  msg: Message
) extends EmailUser {

  def lovable = {
    userEmail != "noreply@rallyhealth.com"
  }
}

trait LoveJsonProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val sendLoveFormat = jsonFormat3(RallyLove.apply)
}
object LoveJsonProtocol extends LoveJsonProtocol
case class RallyLove(
  sender: String,
  recipients: Seq[String],
  message: String
)

trait LoveResultJPTL extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val sendLoveFormat = jsonFormat4(LoveResult.apply)
}
object LoveResultJPTL extends LoveResultJPTL
case class LoveResult(
  sender: String,
  recipients: Seq[String],
  message: String,
  sentDate: Option[Int]
)

trait LoveListJPTL extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val loveResultFormat = jsonFormat4(LoveResult.apply)
  implicit val loveListFormat   = jsonFormat2(LoveList.apply)
}
object LoveListJPTL extends LoveListJPTL
case class LoveList(
  sent: Seq[LoveResult],
  received: Seq[LoveResult]
)

case class Guess(
  msg: Message,
  targets: Seq[String],
  giveUp: Boolean = false
)

object Guess extends BotCombinators {
  def guess[_: P]  = P(IgnoreCase("guess"))
  def reason[_: P] = (!(nick) ~ AnyChar).rep
  def opt[_: P]    = P(guess ~/ nick.rep ~ End)

  def apply(
    msg: Message
  ): Option[Guess] = {
    apply(msg.text, msg)
  }

  def apply(
    str: String,
    msg: Message
  ): Option[Guess] = {
    val gup =
      if (msg.text.toLowerCase.contains("give up") ||
          msg.text.toLowerCase.contains("vito rules") ||
          msg.text.toLowerCase.contains("tell me")) true
      else false
    parse(str, opt(_)) match {
      case Parsed.Success(value, _) => Some(Guess(msg, value, gup))
      case _ =>
        if (gup) Some(Guess(msg, List("abc"), true))
        else None
    }
  }
}

trait RallyLoving extends BotCombinators {

  def love[_: P](
    str: String
  )                = P(IgnoreCase(str))
  def reason[_: P] = (!(nick) ~ AnyChar).rep

  // bot, love for some reason <@abc>
  def opt1[_: P](
    str: String
  ) = P(bot.? ~ love(str) ~/ (reason ~ &(nick)).!.? ~/ nick.rep ~ "!".? ~ End)

  // bot, love <@abc> for some reason
  // i love you <@abc> reasoon
  def opt2[_: P](
    str: String
  ) =
    P(
      (bot | IgnoreCase("i")).? ~ love(str) ~ IgnoreCase("you").? ~/ nick.rep ~/ (reason).!.? ~ "!".? ~ End
    )

  protected def create[T](
    cmd: String,
    str: String,
    msg: Message,
    volume: Int,
    creator: (Message, Seq[String], Option[String], Int) => T
  ): Option[T] = {
    val res1 = parse(str, opt1(cmd)(_)) match {
      case Parsed.Success(value, _) =>
        value._2.length match {
          case 0 => None
          case _ => Some(creator(msg, value._2, value._1, volume))
        }
      case _ =>
        None
    }
    val res2 = parse(str, opt2(cmd)(_)) match {
      case Parsed.Success(value, _) =>
        value._1.length match {
          case 0 => None
          case _ => Some(creator(msg, value._1, value._2, volume))
        }
      case _ => None
    }
    if (res1 != None) res1
    else if (res2 != None) res2
    else None
  }
}
case class Crush(
  msg: Message,
  targets: Seq[String],
  reason: Option[String],
  volume: Int = 0
)

object Crush extends RallyLoving {

  def apply(
    msg: Message
  ): Option[Crush] = {
    apply(msg.text, msg)
  }

  // def apply(msg:Message, targets: Seq[String], reason: Option[String], volume: Int): Crush = {
  //   val tellThem = msg.text.trim.last == '!'
  //   Crush(msg, targets, reason, if(tellThem) 1 else 0)
  // }

  def apply(
    str: String,
    msg: Message
  ): Option[Crush] = {
    val tellThem = msg.text.trim.last == '!'
    create("crush", str, msg, if (tellThem) 1 else 0, Crush.apply)
  }
}

case class Love(
  msg: Message,
  targets: Seq[String],
  reason: Option[String],
  volume: Int = 2
)

object Love extends RallyLoving {

  def apply(
    msg: Message
  ): Option[Love] = {
    apply(msg.text, msg)
  }

  def apply(
    str: String,
    msg: Message
  ): Option[Love] = {
    create("love", str, msg, 2, Love.apply)
  }
}

case class WhoDoLove(
  msg: Message,
  target: String
)

object WhoDoLove extends BotCombinators {
  import fastparse._, NoWhitespace._

  def who[_: P]  = P(IgnoreCase("who") ~ IgnoreCase("m").? ~ ws)
  def does[_: P] = P(IgnoreCase("do") ~ IgnoreCase("es").? ~ ws)
  def love[_: P] = P(IgnoreCase("love") ~ ws)

  // def opt [_: P] = P(bot.? ~ ws ~ who ~ does ~/ (nick | IgnoreCase("i").! | uname) ~ love ~ "?".? ~ End)
  def opt[_: P] =
    P(bot.? ~ ws ~ who ~ does ~/ (nick | IgnoreCase("i").! | uname) ~ ws ~ love ~ "?".? ~ End)

  def apply(
    msg: Message
  ): Option[WhoDoLove] = {
    apply(msg.text, msg)
  }

  def apply(
    str: String,
    msg: Message
  ): Option[WhoDoLove] = {
    parse(str, opt(_)) match {
      case Parsed.Success(value, _) => Some(WhoDoLove(msg, value))
      case _ =>
        None
    }
  }
}

case class WhoLove(
  msg: Message,
  target: String
)

object WhoLove extends BotCombinators {

  def who[_: P] = P(IgnoreCase("who") ~ IgnoreCase("love") ~ IgnoreCase("s").?)
  def opt[_: P] = P(bot.? ~ who ~ (nick | uname | IgnoreCase("me").!) ~ "?".? ~ End)

  def apply(
    msg: Message
  ): Option[WhoLove] = {
    apply(msg.text, msg)
  }

  def apply(
    str: String,
    msg: Message
  ): Option[WhoLove] = {
    parse(str, opt(_)) match {
      case Parsed.Success(value, _) => Some(WhoLove(msg, value))
      case _                        => None
    }
  }
}

// trait WhoUserJPTL extends SprayJsonSupport with DefaultJsonProtocol {
//   implicit val loveResultFormat = jsonFormat4(LoveResult.apply)
//   implicit val loveListFormat = jsonFormat2(WhoUser.apply)
// }
// object WhoUserJPTL extends WhoUserJPTL
// case class WhoUser( sent: Seq[LoveResult], received: Seq[LoveResult] )
