package v2d2.actions.knock

import fastparse._
import fastparse.all._
import fastparse.parsers.Combinators.Rule
import fastparse.parsers._
import v2d2.parsers.{Blackspace,BotCombinators}
import v2d2.client.IMessage
import v2d2.LoggerConfig

case class KnockKnock(target:Option[String])
object KnockKnock extends BotCombinators with LoggerConfig {
  import fastparse.noApi._
  import White._

  val knock: P[Unit] = P(IgnoreCase("knock"))
  val knocker: P[Unit] = P((knock ~  knock.?) ~ ",".? )
  val target: P[Unit] = P(!knock ~ AnyChar.rep(1))
  val com: P[String] = P(bot.? ~ knocker ~ nick ~ End)

  def apply(imsg: IMessage): Option[KnockKnock] = {
    apply(imsg.content)
  }

  def apply(str: String): Option[KnockKnock] = {
    com.parse(str) match {
      case Parsed.Success(value, _) => Some(KnockKnock(Some(value)))
      case _ =>
        // println(com.parse(str))
        None
    }
  }
}

case class Whois(input:Option[String])
object Whois extends BotCombinators {
  import fastparse.noApi._
  import White._
  val who: P[Unit] = P(IgnoreCase("who"))
  val dat: P[Unit] = P("(".? ~ IgnoreCase("dat") ~ ")".?)
  val there: P[Unit] = P(IgnoreCase("there") | IgnoreCase("that") | dat)
  val is: P[Unit] = P(("is"|"'s"))
  val punc: P[Unit] = P(("?"|"!"|"."))
  val com: P[Unit] = P(bot.? ~ who ~ is.? ~ there ~ punc.rep ~ End)
  def apply(imsg: IMessage): Option[Whois] = {
    apply(imsg.content)
  }
  def apply(str: String): Option[Whois] = {
    com.parse(str) match {
      case Parsed.Success(value, _) => Some(Whois(Some(str)))
      case _ =>
        // println(com.parse(str))
        None
    }
  }
}

case class Who(input:Option[String])
object Who extends BotCombinators {
  import fastparse.noApi._
  import White._
  val who: P[Unit] = P(IgnoreCase("who"))
  val punc: P[Unit] = P(("?"|"!"|"."))
  val com: P[String] = P((!who ~ wild).rep.! ~ who ~ punc.rep ~ End)

  def apply(imsg: IMessage): Option[Who] = {
    apply(imsg.content)
  }

  def apply(str: String): Option[Who] = {
    com.parse(str) match {
      case Parsed.Success(value, _) => Some(Who(Some(value)))
      case _ =>
        // println(com.parse(str))
        None
    }
  }
}
