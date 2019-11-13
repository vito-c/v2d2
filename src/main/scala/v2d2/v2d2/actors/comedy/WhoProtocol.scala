package v2d2.actions.who

import spray.json.DefaultJsonProtocol
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.joda.time.DateTime
import fastparse._, SingleLineWhitespace._
import v2d2.actors.core.BotCombinators
import slack.models.Message
import slack.models.User

case class GetWhoUser(
  msg: Message,
  target: User,
  silent: Boolean
)
case class GetWhoAll(
  msg: Message,
  search: String,
  silent: Boolean
)
case class WhoIs(
  msg: Message,
  target: String,
  silent: Boolean
)

object WhoIs extends BotCombinators {

  def who[_: P] = P(IgnoreCase("rally").? ~ ws.? ~ IgnoreCase("who") ~ ws ~ IgnoreCase("is") ~ ws)

  def opt[_: P] = P(bot.? ~ who ~/ (nick | uname) ~ "?".rep ~ End)

  def apply(
    msg: Message
  ): Option[WhoIs] = {
    apply(msg.text, msg)
  }

  def apply(
    str: String,
    msg: Message
  ): Option[WhoIs] = {
    parse(str, opt(_)) match {
      case Parsed.Success(value, _) =>
        value.toLowerCase() match {
          case "there"   => None
          case "their"   => None
          case "it"      => None
          case "on call" => None
          case _         => Some(WhoIs(msg, value, false))
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
