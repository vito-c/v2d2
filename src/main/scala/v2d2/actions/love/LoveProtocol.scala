package v2d2.actions.love

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import fastparse._
import fastparse.all._
import fastparse.core.Parsed
import spray.json.DefaultJsonProtocol
import v2d2.client.{IMessage, User}
import v2d2.parsers.BotCombinators

//'{ "sender": "vito.cutten", "recipients": ["doug.roper@rallyhealth.com"], "message": "no reason" }'
// case class LoveSuccess(
//   sender: User,
//   receivers: Seq[User],
//   reason: String,
//   imsg: IMessage)
case class SendUsersLove(
  sender: User,
  recipients: Seq[User],
  message: Option[String],
  imsg: IMessage)

case class GetUsersSentLove(
  target: User,
  senderNick: String,
  imsg: IMessage)

case class GetUsersLove(
  target: User,
  senderNick: String,
  imsg: IMessage)
trait LoveJsonProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val sendLoveFormat = jsonFormat3(RallyLove.apply)
}
object LoveJsonProtocol extends LoveJsonProtocol
case class RallyLove(
  sender: String,
  recipients: Seq[String],
  message: String)

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
  implicit val loveListFormat = jsonFormat2(LoveList.apply)
}
object LoveListJPTL extends LoveListJPTL
case class LoveList(sent: Seq[LoveResult], received: Seq[LoveResult])

case class Love(targets: Seq[String], reason: Option[String])
object Love extends BotCombinators {
  import White._

  val love: P[Unit] = P(IgnoreCase("love"))
  val reason: P[Unit] = (!at ~ AnyChar).rep
  val opt1: P[(Option[String],Seq[String])] = P(bot.? ~ love ~ reason.!.? ~ (nicks) ~ End)
  val opt2: P[(Seq[String],Option[String])] = P(bot.? ~ love ~ (nicks) ~ reason.!.? ~ End)

  def apply(imsg: IMessage): Option[Love] = {
    apply(imsg.content)
  }

  def apply(str: String): Option[Love] = {
    val res1 = opt1.parse(str) match {
      case Parsed.Success(value, _) =>
        value._2.length match {
          case 0 => None
          case _ => Some(Love(value._2, value._1))
        }
      case _ => None
    }
    val res2 = opt2.parse(str) match {
      case Parsed.Success(value, _) =>
        value._1.length match {
          case 0 => None
          case _ => Some(Love(value._1, value._2))
        }
      case _ => None
    }
    if (res1 != None) res1
    else if (res2 != None) res2
    else None
  }
}

case class WhoDoLove(imsg:IMessage, target: String)
object WhoDoLove extends BotCombinators {
  val ws: P[Unit] = (" "|s"\t").rep.?

  val who: P[Unit] = P(IgnoreCase("who") ~ IgnoreCase("m").? ~ ws)
  val does: P[Unit] = P(IgnoreCase("do") ~ IgnoreCase("es").? ~ ws)
  val letter: P[Unit] = CharIn('A' to 'Z') | CharIn('a' to 'z')
  val target: P[String] = P(at.? ~ letter.rep.! ~ ws)
  val love: P[Unit] = P(IgnoreCase("love") ~ ws)
  val opt: P[String] = P(bot.? ~ ws ~ 
    who ~ does ~ target ~ love ~ "?".? ~ End)

  def apply(imsg: IMessage): Option[WhoDoLove] = {
    apply(imsg.content, imsg)
  }

  def apply(str: String, imsg:IMessage): Option[WhoDoLove] = {
    opt.parse(str) match {
      case Parsed.Success(value, _) => Some(WhoDoLove(imsg, value))
      case _ =>
        None
    }
  }
}
case class WhoLove(imsg:IMessage, target: String)
object WhoLove extends BotCombinators {
  import White._

  val who: P[Unit] = P(IgnoreCase("who love") ~ IgnoreCase("s").?)
  val letter = CharIn('A' to 'Z') | CharIn('a' to 'z')
  // val target: P[String] = P( at.? ~ (IgnoreCase("me") | wild.rep).! ~ " ".?)
  val target: P[String] = P( at.? ~ (letter.rep).! ~ " ".?)
  val opt: P[String] = P(bot.? ~ who ~ target ~ "?".? ~ End)

  def apply(imsg: IMessage): Option[WhoLove] = {
    apply(imsg.content, imsg)
  }

  def apply(str: String, imsg:IMessage): Option[WhoLove] = {
    opt.parse(str) match {
      case Parsed.Success(value, _) => Some(WhoLove(imsg, value))
      case _ => None
    }
  }
}

// trait WhoUserJPTL extends SprayJsonSupport with DefaultJsonProtocol {
//   implicit val loveResultFormat = jsonFormat4(LoveResult.apply)
//   implicit val loveListFormat = jsonFormat2(WhoUser.apply)
// }
// object WhoUserJPTL extends WhoUserJPTL
// case class WhoUser( sent: Seq[LoveResult], received: Seq[LoveResult] )
