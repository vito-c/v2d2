package v2d2.client.core

import v2d2.parsers.{Blackspace,BotCombinators}
import v2d2.client.IMessage
import scala.collection.immutable.Queue

case class JoinRoom(room: String, chatpass: Option[String])
case class RosterList()
case class UserList()
case class UserMap()
case class EmailMap()
case class NickMap()
case class ProfileRQ(jid: String)
case class MakeRosterDirty()
case class Ping()

// case class Again(bang: Option[String], cmd: Option[String], imsg: IMessage)
case class Again(cmd: Option[String], imsg: IMessage)
case class History(msgs: Option[Queue[IMessage]] = None, again: Option[Again])
object Again extends BotCombinators {
  import fastparse.noApi._
  import White._

  /*****************************************************
  * Repeat Last Immediate Command:
  *   - <bot> ! 
  *   - <bot> again
  *   - !!
  *
  * Repeat Last Command Starting With: (ie love)
  *   - <bot> love!
  *   - <bot> love again
  *   - <bot> !love
  *****************************************************/

  val bang: P[Unit] = P(IgnoreCase("!"))
  val again: P[Unit] = P(IgnoreCase("again"))
  val opt: P[Option[String]] = P(bot ~ (bang|again).? ~ (AnyChar.rep).!.? ~ End)
  // val opt1: P[(Option[String],Option[String])] = P(bot.? ~ (bang|again).!.? ~ (AnyChar.rep).!.? ~ End)
  // val opt2: P[(Option[String],Option[String])] = P(bot.? ~ (AnyChar.rep).!.? ~ (bang|again).!.? ~ End)

  def apply(imsg: IMessage): Option[Again] = {
    apply(imsg.content, imsg)
  }

  def apply(str: String, imsg:IMessage): Option[Again] = {
    opt.parse(str) match {
      case Parsed.Success(value, _) => 
        // log.info(s"parse '${value}'")
        println(s"parse '${value}'")
        value match {
          case Some(str) =>
            str.length match {
              case 0 => 
                println(s"len  0")
                Some(Again(None, imsg))
              case _ => 
                println(s"len  not 0")
                Some(Again(value, imsg))
            }
          case _ => 
            println(s"other match")
            Some(Again(None, imsg))
        }
      case _ => None
    }
    // val res2 = opt2.parse(str) match {
    //   case Parsed.Success(value, _) => Some(Again(value._2, value._1))
    //   case _ => None
    // }
    // if (res1 != None) res1
    // else if (res2 != None) res2
    // else None
  }
}
