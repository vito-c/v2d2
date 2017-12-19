package v2d2.mtg

import fastparse._
import fastparse.all._
import fastparse.core.Parsed
import spray.json.DefaultJsonProtocol
import v2d2.client.{IMessage, User}
import v2d2.parsers.BotCombinators

case class CardNameSearch(imsg:IMessage, target: String)
object CardNameSearch extends BotCombinators {
  // v2d2, what card name is foo bar?
  // v2d2, what card name is foo bar with text?
  // v2d2, what card name is foo bar with text?
  // card name foo bar
  // card name foo bar (with text|+text|--text|-t)
  // !card name foo bar
  val word = P(letter | CharIn("`',-_$\""))
  val target: P[String] = P((word ~ (" "|s"\t").rep).rep(1).!)
    // ( (letter.rep ~ (" "|s"\t").rep(1) ~ letter.rep).! |
    //   (letter.rep(1)).! ) ~ " ".?)
    // what card name is foo bar
  val card: P[Unit] = P(
    ic("what").? ~ ws ~ 
    ic("card") ~ ws ~ 
    ic("name") ~ ws ~
    ic("is").? ~ ws)
  val text:P[Unit] = P(ic("text"))
  // wait to implement
  val optText:P[Unit] = P(
    ic("with").? ~ ws ~
    ((CharIn("+")|"--") ~ text |
    CharIn("-") ~ "t"))
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
