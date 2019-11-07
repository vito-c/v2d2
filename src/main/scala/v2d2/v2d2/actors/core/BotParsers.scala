package v2d2.actors.core

import fastparse._, NoWhitespace._
import v2d2.V2D2
// import fastparse.all._
// import fastparse.parsers._

// trait ISearchable{
//   def needle: String
//   def asker: Option[ActorRef] }
// case class Email(needle: String, asker: Option[ActorRef] = None) extends ISearchable

// case class Nick(needle: String) extends ISearchable
// case class FullName(needle: String) extends ISearchable
// case class UName(needle: String) extends ISearchable
// case class Name(needle: String) extends ISearchable

trait BotCombinators {
  // val symbols = P(CharIn(' ' to '/')|CharIn(':' to '@')|CharIn('[' to '`')|CharIn('{' to '~'))
  // val ltrs = P(letter.rep(1))
  // val alphanum = letter | CharIn('0' to '9') //CharIn('A' to 'Z') | CharIn('a' to 'z') | CharIn('0' to '9')
  // val txt = P(alphanum | CharIn(".`',-_$\""))
  // val txts = P(txt.rep(1))
  // val wnick: P[String] = P((at ~ alphanum.rep(1)).!)
  // val znick: P[String] =P(at.? ~/ ((!(symbols) ~ AnyChar).rep).!)
  //
  // val jid: P[String] = P((txts ~ at ~ "chat.btf.hipchat.com").!)
  // val email: P[String] = P((txts ~ at ~ alphanum.rep(2) ~ "." ~ ltrs).!)
  // val nicky: P[String] = P(at ~ alphanum.rep(1).!)
  // val fname: P[String] = P((letter.rep(2) ~ ws ~ letter.rep(2)).!)
  // val uname: P[String] = P((letter.rep(2) ~ "." ~ letter.rep(1)).!)
  // val name:  P[String] = P((letter.rep(1)).!)
  //
  // val ajid: P[JID]        = P(jid.map(JID(_)))
  // val aemail: P[Email]    = P(email.map(Email(_)))
  // val anicky: P[Nick]     = P(nicky.map(Nick(_)))
  // val afname: P[FullName] = P(fname.map(FullName(_)))
  // val auname: P[UName]    = P(uname.map(UName(_)))
  // val aname:  P[Name]     = P(name.map(Name(_)))

  def ws[_: P]     = P((" " | s"\t").rep.?)
  def letter[_: P] = P(CharIn("A-Z") | CharIn("a-z"))

  def ic[_: P](
    str: String
  ) = IgnoreCase(str)

  def at[_: P]    = P("@")
  def lt[_: P]    = P("<")
  def gt[_: P]    = P(">")
  def bnick[_: P] = P(lt ~ at ~/ V2D2.selfId ~ gt)

  def bot[_: P] =
    P((bnick | IgnoreCase("bot") | IgnoreCase("v2d2") | IgnoreCase("!") | IgnoreCase("/")) ~ ",".?)
  def nick[_: P] = P(lt ~ at ~/ (!gt ~ AnyChar).rep(1).! ~ gt)

  def uname[_: P] =
    P((letter.rep(1) ~ ("." | " ") ~ letter.rep(1) ~ ("@" ~ IgnoreCase("rallyhealth.com")).?).!)

  def nicks[_: P]: P[Seq[String]] = P(nick.rep)
}
