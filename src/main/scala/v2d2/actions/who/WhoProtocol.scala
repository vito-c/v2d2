package v2d2.actions.who

import fastparse._
import fastparse.core.Parsed
import fastparse.all._
import fastparse.parsers.Combinators.Rule
import fastparse.parsers._
import v2d2.client.IMessage
import v2d2.parsers.AutoParser
import v2d2.parsers.{Blackspace,BotCombinators}
import spray.json.DefaultJsonProtocol
import v2d2.client.{IMessage,User}
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.joda.time.DateTime

case class GetWhoUser(imsg: IMessage, target: User)
case class GetWhoAll(imsg: IMessage, search: String)
case class WhoIs(imsg:IMessage, target: String)
object WhoIs extends BotCombinators {
  val who: P[Unit] = P(IgnoreCase("who") ~ ws ~ IgnoreCase("is") ~ ws)
  val target: P[String] = P( 
    ((letter.rep ~ "." ~ letter.rep).! ~ "@".? ~ AnyChar.rep | 
      (letter.rep ~ " " ~ letter.rep).! |
      at.? ~/ ((!(symbols) ~ AnyChar).rep).!) ~/ " ".?)
  val opt: P[String] = P(bot.? ~ who ~ target ~ "?".rep ~ End)

  def apply(imsg: IMessage): Option[WhoIs] = {
    apply(imsg.content, imsg)
  }

  def apply(str: String, imsg:IMessage): Option[WhoIs] = {
    opt.parse(str) match {
      case Parsed.Success(value, _) => 
        value.toLowerCase() match {
          case "there" => None
          case "their" => None
          case "it" => None
          case "on call" => None
          case _ => Some(WhoIs(imsg, value))
        }
      case _ =>
        None
    }
  }
}
trait WhoJPTL extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val sendWhoFormat = jsonFormat18(WhoUser.apply)
}
object WhoJPTL extends WhoJPTL
case class WhoUser(
  id: String,
  name: String,
  first: String,
  otherNames: Option[Set[String]],
  email: String,
  gender: Option[String],
  avatar: Option[String],
  loc: Option[String],
  role: String,
  team: Option[String],
  dept: Option[String],
  start: String,
  manager: Option[String],
  reports: Option[Seq[String]],
  hipchatMention: Option[String],
  gitHubUsername: Option[String],
  linkedInId: Option[String],
  contributor: Boolean
) {
  def last: String = { name.split(" ").last }
}
