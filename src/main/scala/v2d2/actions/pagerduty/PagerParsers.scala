package v2d2.actions.pager

import fastparse._
import fastparse.all._
import fastparse.core.Parsed
import v2d2.client.IMessage
import v2d2.parsers.BotCombinators

case class WhenOnCall(imsg:IMessage, target: String)
case class WhoOnCall(target: Option[String], date: Option[String])
case class OnCallTeams(imsg: IMessage, teams: Option[Seq[String]])

object OnCallTeams extends BotCombinators {
  //what are the (on ?call|pager(duty)?) teams
  //is foo an (on call|pager(duty)?) team
  //are foo, biz, and.? baz on call teams
  //are foo and.? baz on call teams

  val ss: P[Unit] = P((" "|s"\t").rep)
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

//when is <target> on call?
//  on call <target>
//  <target> on call
//when am I on call
object WhenOnCall extends BotCombinators {
  val whis: P[Unit] = P(IgnoreCase("when") ~ ws ~ (IgnoreCase("is")|IgnoreCase("am")) ~ ws)
  val oncall: P[Unit] = P(
    IgnoreCase("on") ~ ws ~ 
    IgnoreCase("call") ~ ws)
  val target: P[String] = P(email|znick|fname|uname|IgnoreCase("i").!)

  val opt: P[String] = P(
    bot.? ~ whis.? ~ 
    ((target ~ ws ~ oncall)|(oncall ~ target)) ~ "?".rep ~ End)

  def apply(imsg: IMessage): Option[WhenOnCall] = {
    apply(imsg.content, imsg)
  }

  def apply(str: String, imsg: IMessage): Option[WhenOnCall] = {
    opt.parse(str) match {
      case Parsed.Success(value, _) => 
        pprint.log(value)
        Some(WhenOnCall(imsg, value))
      case _ => None
    }
  }

}


//who is on call <date>?
//  on call <date>
//who is on call for <summary/team> on <date>
//  on call <summary/team> <date>
object WhoOnCall extends BotCombinators {
  val who: P[Unit] = P(IgnoreCase("who") ~ ws ~ IgnoreCase("is") ~ ws)
  val oncallfor: P[Unit] = P(
    IgnoreCase("on") ~ ws ~ 
    IgnoreCase("call") ~ ws.? ~
    IgnoreCase("for").? ~ ws)
//   val alphanum = CharIn('A' to 'Z') | CharIn('a' to 'z') | CharIn('0' to '9')
  val date = P(alphanum|"/"|"-"|"."|":"|" "|",")
  val team: P[Option[String]] = P((alphanum.rep(min=1)).!.? ~ ws)
  val on = P(IgnoreCase("on") ~ ws)
  val time: P[Option[String]] = P(on.? ~ (date.rep(min=1)).!.? ~ ws)
  val opt: P[(Option[String],Option[String])] = P(bot.? ~ who.? ~ oncallfor ~ team ~ time ~ "?".rep ~ End)


  def apply(imsg: IMessage): Option[WhoOnCall] = {
    apply(imsg.content)
  }

  def apply(str: String): Option[WhoOnCall] = {
    opt.parse(str) match {
      case Parsed.Success(value, _) => 
        Some(WhoOnCall(value._1, value._2))
      case _ => None
    }
  }
}
