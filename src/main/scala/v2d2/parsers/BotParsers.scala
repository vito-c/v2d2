package v2d2.parsers

import fastparse._
import fastparse.all._
import fastparse.parsers.Combinators.Rule
import fastparse.parsers._
import akka.actor.ActorRef
import v2d2.client.core._

// trait ISearchable{ 
//   def needle: String
//   def asker: Option[ActorRef] }
// case class Email(needle: String, asker: Option[ActorRef] = None) extends ISearchable

// case class Nick(needle: String) extends ISearchable
// case class FullName(needle: String) extends ISearchable
// case class UName(needle: String) extends ISearchable
// case class Name(needle: String) extends ISearchable

trait BotCombinators {
  val ws: P[Unit] = P((" "|s"\t").rep.?)
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  val symbols = P(CharIn(' ' to '/')|CharIn(':' to '@')|CharIn('[' to '`')|CharIn('{' to '~'))
  def ic(str:String) = { IgnoreCase(str) }
  val at: P[Unit] = P("@")
  val letter = CharIn('A' to 'Z') | CharIn('a' to 'z')
  val ltrs = P(letter.rep(1))
  val alphanum = letter | CharIn('0' to '9') //CharIn('A' to 'Z') | CharIn('a' to 'z') | CharIn('0' to '9')
  val txt = P(alphanum | CharIn(".`',-_$\"")) 
  val txts = P(txt.rep(1))
  val wnick: P[String] = P((at ~ alphanum.rep(1)).!)
  val znick: P[String] =P(at.? ~/ ((!(symbols) ~ AnyChar).rep).!)

  val jid: P[String] = P((txts ~ at ~ "chat.btf.hipchat.com").!)
  val email: P[String] = P((txts ~ at ~ alphanum.rep(2) ~ "." ~ ltrs).!)
  val nicky: P[String] = P(at ~ alphanum.rep(1).!)
  val fname: P[String] = P((letter.rep(2) ~ ws ~ letter.rep(2)).!)
  val uname: P[String] = P((letter.rep(2) ~ "." ~ letter.rep(1)).!)
  val name:  P[String] = P((letter.rep(1)).!)

  val ajid: P[JID]        = P(jid.map(JID(_)))
  val aemail: P[Email]    = P(email.map(Email(_)))
  val anicky: P[Nick]     = P(nicky.map(Nick(_)))
  val afname: P[FullName] = P(fname.map(FullName(_)))
  val auname: P[UName]    = P(uname.map(UName(_)))
  val aname:  P[Name]     = P(name.map(Name(_)))

  val bot: P[Unit] = P((ic("bot") | ic("v2d2") | ic("!")) ~ ",".?)

  // val wild = P(CharPred(x => Blackspace.matches(x)))
  val nick: P[String] = P(at ~ wild.rep.! ~ " ".?)
  val nicks: P[Seq[String]] = P(nick.rep)
}

// object Blackspace {
//   private val wTable: String =
//     "\u2002\u3000\r\u0085\u200A\u2005\u2000\u3000" +
//       "\u2029\u000B\u3000\u2008\u2003\u205F\u3000\u1680" +
//       "\u0009\u0020\u2006\u2001\u202F\u00A0\u000C\u2009" +
//       "\u3000\u2004\u3000\u3000\u2028\n\u2007\u3000"
//
//   private val Multiplier: Int = 1682554634
//   private val Shift: Int = Integer.numberOfLeadingZeros(wTable.length - 1)
//
//   def matches(c: Char): Boolean = wTable.charAt((Multiplier * c) >>> Shift) != c
// }
