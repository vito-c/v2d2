package v2d2.parsers

import fastparse._
import fastparse.all._
import fastparse.parsers.Combinators.Rule
import fastparse.parsers._

trait BotCombinators {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  val bot: P[Unit] = P((IgnoreCase("bot") | IgnoreCase("v2d2") | IgnoreCase("!")) ~ ",".?)

  val at: P[Unit] = P("@")
  val wild = P(CharPred(x => Blackspace.matches(x)))
  val nick: P[String] = P(at ~ wild.rep.! ~ " ".?)
  val nicks: P[Seq[String]] = P(nick.rep)
}

object Blackspace {
  private val wTable: String =
    "\u2002\u3000\r\u0085\u200A\u2005\u2000\u3000" +
      "\u2029\u000B\u3000\u2008\u2003\u205F\u3000\u1680" +
      "\u0009\u0020\u2006\u2001\u202F\u00A0\u000C\u2009" +
      "\u3000\u2004\u3000\u3000\u2028\n\u2007\u3000"

  private val Multiplier: Int = 1682554634
  private val Shift: Int = Integer.numberOfLeadingZeros(wTable.length - 1)

  def matches(c: Char): Boolean = wTable.charAt((Multiplier * c) >>> Shift) != c
}

