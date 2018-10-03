package v2d2.actions.knock

import fastparse._
import fastparse.core.Parsed
import fastparse.all._
import fastparse.parsers.Combinators.Rule
import fastparse.parsers._
import v2d2.parsers.{Blackspace,BotCombinators}
import org.jxmpp.jid.BareJid
import v2d2.client.IMessage
import v2d2.LoggerConfig

case class Retort(
  jid:Option[String],
  user:Option[String],
  sender:Option[String],
  imsg:Option[IMessage])

case class KnockKnock(imsg:IMessage, target:Option[String])
object KnockKnock extends BotCombinators with LoggerConfig {
  // import fastparse.noApi._
  import White._

  val knock: P[Unit] = P(IgnoreCase("knock"))
  val knocker: P[Unit] = P((knock ~  knock.?) ~ ",".? )
  val com: P[String] = P(bot.? ~ knocker ~ nick ~ End)

  def apply(imsg: IMessage): Option[KnockKnock] = {
    apply(imsg.content, imsg)
  }

  def apply(str: String, imsg:IMessage): Option[KnockKnock] = {
    com.parse(str) match {
      case Parsed.Success(value, _) => Some(KnockKnock(imsg, Some(value)))
      case _ =>
        // println(com.parse(str))
        None
    }
  }
}

case class Whois(imsg:IMessage, input:Option[String])
object Whois extends BotCombinators {
  // import fastparse.noApi._
  import White._
  val who: P[Unit] = P(IgnoreCase("who"))
  val dat: P[Unit] = P("(".? ~ IgnoreCase("dat") ~ ")".?)
  val there: P[Unit] = P(IgnoreCase("there") | IgnoreCase("that") | dat)
  val is: P[Unit] = P(("is"|"'s"))
  val punc: P[Unit] = P(("?"|"!"|"."))
  val com: P[Unit] = P(bot.? ~ who ~ is.? ~ there ~ punc.rep ~ End)

  def apply(imsg: IMessage): Option[Whois] = {
    apply(imsg.content, imsg)
  }
  def apply(str: String, imsg:IMessage): Option[Whois] = {
    com.parse(str) match {
      case Parsed.Success(value, _) => Some(Whois(imsg, Some(str)))
      case _ =>
        // println(com.parse(str))
        None
    }
  }
}

case class Who(imsg:IMessage, input:Option[String])
object Who extends BotCombinators {
  // import fastparse.noApi._
  import White._
  val who: P[Unit] = P(IgnoreCase("who"))
  val punc: P[Unit] = P(("?"|"!"|"."))
  val com: P[String] = P((!who ~ wild).rep.! ~ who ~ punc.rep ~ End)

  def apply(imsg: IMessage): Option[Who] = {
    apply(imsg.content, imsg)
  }

  def apply(str: String, imsg: IMessage): Option[Who] = {
    com.parse(str) match {
      case Parsed.Success(value, _) => Some(Who(imsg, Some(value)))
      case _ =>
        // println(com.parse(str))
        None
    }
  }
}
