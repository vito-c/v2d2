package v2d2.actions.generic.protocol

import v2d2.actions.generic.HipNotif
import v2d2.client.IMessage
import fastparse._
import fastparse.core.Parsed
import fastparse.all._
import fastparse.parsers.Combinators.Rule
import fastparse.parsers._
import v2d2.parsers.AutoParser
import v2d2.parsers.{Blackspace,BotCombinators}
import spray.json.DefaultJsonProtocol
import v2d2.client.{IMessage,User}
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport

case class Response(originalMsg: IMessage, response: String, notification:Option[HipNotif] = None)
case class Relay(imsg: IMessage)
// case class ProfileReq(target: String)

case class Helpme(imsg: Option[IMessage])
object Helpme extends AutoParser[Helpme]

case class Nailed(imsg: Option[IMessage])
object Nailed extends AutoParser[Nailed]

case class Quit(imsg: Option[IMessage])
object Quit extends AutoParser[Quit]

// case class Lang(type: String,
//just going to parse the reason again.


// Usage: !servers environment role or !servers find hostname
// or !servers find ipaddress or !servers find fqdn
// Example:
//   !servers rally-demo
//   !servers rally-demo api
//   !servers all api
//   !servers find api-i-48a9909a
//   !servers find 48a99
//   !servers find 10.71.32.148
//   !servers find mdb1.rally-demo.werally.in
case class Server(servers: Seq[String])
object Server extends BotCombinators {
  import White._

  val server: P[Unit] = P(IgnoreCase("server") ~ "s".?)
  val target: P[String] = P(wild.rep(1).! ~ " ".?)
  // val d: P[Unit] = P(CharIn('0' to '9').rep)
  // val ip: P[String] = P((d ~ "." ~ d ~ "." ~ d ~ "." ~ d).! ~ End)
  val targets: P[Seq[String]] = P(target.rep)

  val opt: P[Seq[String]] = P(bot.? ~ server ~ (targets) ~ End)

  def apply(imsg: IMessage): Option[Server] = {
    apply(imsg.content)
  }

  def apply(str: String): Option[Server] = {
    opt.parse(str) match {
      case Parsed.Success(value, _) =>
            Some(Server(value))
      case _ => None
    }
  }
}
