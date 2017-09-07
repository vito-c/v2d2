package v2d2.actions.pager

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

case class GetAllOnCall(imsg:IMessage, offset:Int = 0, limit:Int = 100)

trait PagerOnCallSeqProtocol 
  extends SprayJsonSupport 
  with DefaultJsonProtocol {
  implicit val pagerUserFormat = jsonFormat4(PagerUser.apply)
  implicit val escalationFormat = jsonFormat4(EscalationPolicy.apply)
  implicit val pagerOnCallFormat = jsonFormat3(PagerOnCall.apply)
  implicit val onCallsFormat = jsonFormat3(PagerOnCallSeq.apply)
}
object PagerOnCallSeqProtocol extends PagerOnCallSeqProtocol
case class PagerOnCallSeq(oncalls: Seq[PagerOnCall], limit: Int, more: Boolean)

trait PagerOnCallProtocol 
  extends SprayJsonSupport 
  with DefaultJsonProtocol {
  implicit val pagerUserFormat = jsonFormat4(PagerUser.apply)
  implicit val escalationFormat = jsonFormat4(EscalationPolicy.apply)
  implicit val pagerOnCallFormat = jsonFormat3(PagerOnCall.apply)
}
object PagerOnCallProtocol extends PagerOnCallProtocol
case class PagerOnCall(
  escalation_policy: EscalationPolicy,
  escalation_level: Int,
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


case class OnCall(team: Option[String], date: Option[String])
case class OnCallTeams(imsg: IMessage, teams: Option[Seq[String]])
object OnCallTeams extends BotCombinators {
  //what are the (on ?call|pager(duty)?) teams
  //is foo an (on call|pager(duty)?) team
  //are foo, biz, and.? baz on call teams
  //are foo and.? baz on call teams

  val ws: P[Unit] = P((" "|s"\t").rep.?)
  val ss: P[Unit] = P((" "|s"\t").rep)
  def ic(str:String) = { IgnoreCase(str) }
  val alphanum = CharIn('A' to 'Z') | CharIn('a' to 'z') | CharIn('0' to '9')
  val whatIs: P[Unit] = P((ic("what").? ~ ws ~ ic("are") ~ ws ~ ic("the").? )|
    ic("is"))
  val on:P[Unit] = P(ic("on"))
  val pager:P[Unit] = P(ic("pager"))
  val oncall: P[Unit] = P( ((on ~ ws ~ ic("call")) | (pager ~ ws ~ ic("duty").?)))
  val article = P(ic("a") ~ ic("n").? ~ ic("d").?)
  val target: P[String] = P( 
    !(on|pager|article) ~ ss ~ alphanum.rep(min=1).! ~ ",".? ~ ss.? ~ article.? ~ ss.?)

  val teams: P[Unit] = P(ic("team") ~ ic("s").?)

  val opt: P[Option[Seq[String]]] = P(bot.? ~ 
    whatIs.? ~ ws ~ target.rep(min=1).? ~ ws ~ article.? ~ ws ~ oncall ~ ws ~ teams ~ "?".rep ~ End)

  def apply(imsg: IMessage): Option[OnCallTeams] = {
    apply(imsg.content, imsg)
  }

  def apply(str: String, imsg: IMessage): Option[OnCallTeams] = {
    opt.parse(str) match {
      case Parsed.Success(value, _) => 
        Some(OnCallTeams(imsg, value))
      case _ => None
    }
  }
}
object OnCall extends BotCombinators {
  val ws: P[Unit] = P((" "|s"\t").rep.?)
  val who: P[Unit] = P(IgnoreCase("who") ~ ws ~ IgnoreCase("is") ~ ws)
  val oncallfor: P[Unit] = P(
    IgnoreCase("on") ~ ws ~ 
    IgnoreCase("call") ~ ws.? ~
    IgnoreCase("for").? ~ ws)
  val alphanum = CharIn('A' to 'Z') | CharIn('a' to 'z') | CharIn('0' to '9')
  val date = P(alphanum|"/"|"-"|"."|":"|" "|",")
  val team: P[Option[String]] = P((alphanum.rep(min=1)).!.? ~ ws)
  val on = P(IgnoreCase("on") ~ ws)
  val time: P[Option[String]] = P(on.? ~ (date.rep(min=1)).!.? ~ ws)
  val opt: P[(Option[String],Option[String])] = P(bot.? ~ who.? ~ oncallfor ~ team ~ time ~ "?".rep ~ End)


  def apply(imsg: IMessage): Option[OnCall] = {
    apply(imsg.content)
  }

  def apply(str: String): Option[OnCall] = {
    opt.parse(str) match {
      case Parsed.Success(value, _) => 
        Some(OnCall(value._1, value._2))
      case _ => None
    }
  }
}
