package v2d2.mtg

import fastparse._
import fastparse.all._
import fastparse.core.Parsed
import spray.json.DefaultJsonProtocol
import v2d2.client.{IMessage, User}
import v2d2.parsers.BotCombinators

case class CardNameSearch(imsg:IMessage, target: String)
object CardNameSearch extends BotCombinators {
  // v2d2, what card is foo bar?
  // card foo bar
  // !card foo bar
  val ws: P[Unit] = P((" "|s"\t").rep.?)
  val letter = P(
    CharIn('A' to 'Z') | 
    CharIn('a' to 'z') | 
    CharIn("`',-_$\""))
  val target: P[String] = P((letter ~ (" "|s"\t").rep).rep(1).!)
    // ( (letter.rep ~ (" "|s"\t").rep(1) ~ letter.rep).! |
    //   (letter.rep(1)).! ) ~ " ".?)
  val card: P[Unit] = P(
    IgnoreCase("what").? ~ ws ~ 
    IgnoreCase("card") ~ ws ~ 
    IgnoreCase("name") ~ ws ~
    IgnoreCase("is").? ~ ws)
  val opt: P[String] = P(bot.? ~ ws ~ card ~ target ~ "?".rep ~ End)

  def apply(imsg: IMessage): Option[CardNameSearch] = {
    println(s"imsg content: ${imsg.content}")
    apply(imsg.content, imsg)
  }

  def apply(str: String, imsg:IMessage): Option[CardNameSearch] = {
    opt.parse(str) match {
      case Parsed.Success(value, _) => 
        Some(CardNameSearch(imsg, value))
      case _ =>
        None
    }
  }

}
