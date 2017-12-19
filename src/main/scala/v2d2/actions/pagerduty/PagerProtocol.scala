package v2d2.actions.pager

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.joda.time.DateTime
import org.joda.time.format.{ISODateTimeFormat, DateTimeFormatter}
import spray.json._
import v2d2.client.IMessage

case class GetAllOnCall(imsg:IMessage, offset:Int = 0, limit:Int = 100)
case class GetOnCall(user: PagerUser)
case class GetPagerUser(email: String)

trait Limiter {
  def limit: Int
  def offset: Int
  def more: Boolean
}

trait PagerOnCallsProtocol 
  extends SprayJsonSupport 
  with DefaultJsonProtocol {

  implicit val pagerUserFormat = jsonFormat4(PagerUser.apply)
  implicit val escalationFormat = jsonFormat4(EscalationPolicy.apply)

  implicit object DateTimeFormat extends RootJsonFormat[org.joda.time.DateTime] {

    val formatter = ISODateTimeFormat.basicDateTimeNoMillis

    def write(obj: DateTime): JsValue = {
      JsString(formatter.print(obj))
    }

    def read(json: JsValue): DateTime = json match {
      case JsString(s) => try {
        println(s"date $s")
        DateTime.parse(s)
      }
      catch {
        case t: Throwable => error(s)
      }
      case _ =>
        error(json.toString())
      // case JsString(s) => try {
      //   formatter.parseDateTime(s)
      // }
      // catch {
      //   case t: Throwable => error(s)
      // }
      //   case _ =>
      //     error(json.toString())
    }

    def error(v: Any): DateTime = {
      val example = formatter.print(0)
      deserializationError(f"'$v' is not a valid date value. Dates must be in compact ISO-8601 format, e.g. '$example'")
    }
  }
  implicit val pagerOnCallFormat = jsonFormat5(PagerOnCall.apply)
  implicit val onCallsFormat = jsonFormat4(PagerOnCalls.apply)

}

object PagerOnCallsProtocol extends PagerOnCallsProtocol
case class PagerOnCalls(
  oncalls: Seq[PagerOnCall],
  limit: Int,
  more: Boolean,
  offset: Int
) extends Limiter

trait PagerUsersProtocol
extends SprayJsonSupport
with DefaultJsonProtocol {
  implicit val pagerUserFormatA = jsonFormat4(PagerUser.apply)
  implicit val pagerUsersFormat = jsonFormat4(PagerUsers.apply)
}
object PagerUsersProtocol extends PagerUsersProtocol
case class PagerUsers(
  users: Seq[PagerUser],
  limit: Int,
  more: Boolean,
  offset: Int
) extends Limiter

trait PagerOnCallProtocol 
extends SprayJsonSupport 
with DefaultJsonProtocol {
  implicit val pagerUserFormat = jsonFormat4(PagerUser.apply)
  implicit val escalationFormat = jsonFormat4(EscalationPolicy.apply)

  implicit object DateTimeFormat extends RootJsonFormat[org.joda.time.DateTime] {

    val formatter = ISODateTimeFormat.basicDateTimeNoMillis

    def write(obj: DateTime): JsValue = {
      JsString(formatter.print(obj))
    }

    def read(json: JsValue): DateTime = json match {
      case JsString(s) => try {
          println(s"date $s")
          DateTime.parse(s)
        } catch {
          case t: Throwable => error(s)
        }
      case _ =>
        error(json.toString())
      // case JsString(s) => try {
      //   formatter.parseDateTime(s)
      // }
      // catch {
      //   case t: Throwable => error(s)
      // }
      //   case _ =>
      //     error(json.toString())
    }

    def error(v: Any): DateTime = {
      val example = formatter.print(0)
      deserializationError(f"'$v' is not a valid date value. Dates must be in compact ISO-8601 format, e.g. '$example'")
    }
  }
  implicit val pagerOnCallFormat = jsonFormat5(PagerOnCall.apply)
}


object PagerOnCallProtocol extends PagerOnCallProtocol
case class PagerOnCall(
  escalation_policy: EscalationPolicy,
  escalation_level: Int,
  start: DateTime,
  end: DateTime,
  user: PagerUser)

trait EscalationPolicyProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val escalationFormat = jsonFormat4(EscalationPolicy.apply)
}

object EscalationPolicyProtocol extends EscalationPolicyProtocol
case class EscalationPolicy(
  id: String,
  // _type: String,
  summary: String,
  self: String,
  html_url: String)

trait PagerUserProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val pagerUserFormat = jsonFormat4(PagerUser.apply)
}
object PagerUserProtocol extends PagerUserProtocol
case class PagerUser(
  id: String,
  // _type: String,
  summary: String,
  self: String,
  html_url: String)
