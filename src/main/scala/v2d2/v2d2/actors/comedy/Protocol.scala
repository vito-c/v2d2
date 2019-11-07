package v2d2.actors.comedy

import fastparse._, SingleLineWhitespace._
import v2d2.actors.core.BotCombinators
import slack.models.Message

case class GetTargets()
case class GetResponses()
case class Targets(
  trgs: Map[String, Joke]
)
case class Responses(
  trgs: Map[String, NewJoke]
)

case class Joke(
  target: String,
  sender: String,
  state: Int = 0,
  tries: Int = 0,
  jokeIdx: Int = 0
)

case class NewJoke(
  target: String,
  sender: String,
  state: Int = 0,
  tries: Int = 0,
  jokeIdx: Int = 0,
  joke: Tuple2[String, String]
)
case class Knockit(
  msg: Message,
  input: Option[String]
)

object Knockit extends BotCombinators {
  def knock[_: P]   = P(IgnoreCase("knock"))
  def knocker[_: P] = P(knock ~ knock)
  def com[_: P]     = P(bot.? ~ knocker ~ End)

  def apply(
    msg: Message
  ): Option[Knockit] = {
    apply(msg.text, msg)
  }

  def apply(
    str: String,
    msg: Message
  ): Option[Knockit] = {
    parse(str, com(_)) match {
      case Parsed.Success(_, _) =>
        Some(Knockit(msg, Some(str)))
      case _ =>
        None
    }
  }
}

case class KnockKnock(
  msg: Message,
  target: Option[String]
)

object KnockKnock extends BotCombinators {
  def knock[_: P]   = P(IgnoreCase("knock"))
  def knocker[_: P] = P(knock ~ knock.? ~ ",".?)
  def com[_: P]     = P(bot.? ~ knocker ~/ nick)

  def apply(
    msg: Message
  ): Option[KnockKnock] = {
    apply(msg.text, msg)
  }

  def apply(
    str: String,
    msg: Message
  ): Option[KnockKnock] = {
    parse(str, com(_)) match {
      case Parsed.Success(value, _) => Some(KnockKnock(msg, Some(value)))
      case _ =>
        None
    }
  }
}

case class Whois(
  msg: Message,
  input: Option[String]
)

object Whois extends BotCombinators {
  def who[_: P]   = P(ic("who"))
  def dat[_: P]   = P("(".? ~ ic("dat") ~ ")".?)
  def there[_: P] = P(ic("there") | ic("that") | dat)
  def is[_: P]    = P(ic("is") | ("’" | "'") ~ ic("s"))
  def punc[_: P]  = P(("?" | "!" | "."))
  def com[_: P]   = P(bot.? ~ who ~ is.? ~ there ~ punc.rep ~ End)

  def apply(
    msg: Message
  ): Option[Whois] = {
    apply(msg.text, msg)
  }

  def apply(
    str: String,
    msg: Message
  ): Option[Whois] = {
    parse(str, com(_)) match {
      case Parsed.Success(value, _) => Some(Whois(msg, Some(str)))
      case _                        =>
        // println(com.parse(str))
        None
    }
  }
}

case class Who(
  msg: Message,
  input: Option[String]
)

object Who {
  def who[_: P]  = P(IgnoreCase("who"))
  def punc[_: P] = P(("?" | "!" | "."))
  def com[_: P]  = P((!who ~ AnyChar).rep(1).! ~ who ~ punc.rep)

  def apply(
    msg: Message
  ): Option[Who] = {
    apply(msg.text, msg)
  }

  def apply(
    str: String,
    msg: Message
  ): Option[Who] = {
    parse(str, com(_)) match {
      case Parsed.Success(value, _) => Some(Who(msg, Some(value)))
      case _                        => None
    }
  }
}
