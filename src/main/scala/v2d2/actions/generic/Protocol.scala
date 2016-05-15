package v2d2.actions.generic.protocol

import v2d2.client.IMessage
import v2d2.parsers.AutoParser
import v2d2.parsers.{Blackspace,BotCombinators}
import spray.json.DefaultJsonProtocol
import v2d2.client.{IMessage,User}
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
case class Relay(imsg: IMessage)
case class ProfileReq(target: String)

case class Helpme(imsg: Option[IMessage])
object Helpme extends AutoParser[Helpme]

case class Nailed(imsg: Option[IMessage])
object Nailed extends AutoParser[Nailed]

case class Quit(imsg: Option[IMessage])
object Quit extends AutoParser[Quit]

//just going to parse the reason again.
case class LoveSuccess(
  sender: User,
  receivers: Seq[User],
  reason: String)
case class SendUsersLove(
  sender: User, receivers: Seq[User], reason: Option[String])
case class SendLoves(
  sender: User,
  receivers: Seq[User],
  senderNick: String,
  receiverNicks: Seq[String],
  reason: String)

// case class SendLove(
//   sender: User,
//   receiver: User,
//   senderNick: String,
//   receiverNick: String,
//   reason: String)
// case class SendUserLove(sender: User, receiver: User, reason: Option[String])

case class SendLoveResult(status: String, error: String, info: String)
case class LoveNickResult(success: Boolean, nickname: String)

trait LoveJsonProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val loveNickFormat = jsonFormat2(LoveNickResult)
}
object LoveJsonProtocol2 extends DefaultJsonProtocol {
  implicit val sendLoveFormat = jsonFormat3(SendLoveResult)
}

case class Love(targets: Seq[String], reason: Option[String])
object Love extends BotCombinators {
  import fastparse.noApi._
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
      case Parsed.Success(value, _) => Some(Love(value._2, value._1))
      case _ =>
        println(opt1.parse(str))
        None
    }
    val res2 = opt2.parse(str) match {
      case Parsed.Success(value, _) => Some(Love(value._1, value._2))
      case _ =>
        println(opt2.parse(str))
        None
    }
    if (res1 != None) res1
    else if (res2 != None) res2
    else None
  }
}
