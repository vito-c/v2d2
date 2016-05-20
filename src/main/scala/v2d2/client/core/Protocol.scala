package v2d2.client.core

import v2d2.parsers.{Blackspace,BotCombinators}
import v2d2.client.IMessage

case class JoinRoom(room: String, chatpass: Option[String])
case class RosterList()
case class UserList()
case class UserMap()
case class NickMap()
case class ProfileRQ(jid: String)
case class MakeRosterDirty()
case class Test()

// case class History(str:String, count:Int, cmd:Option[String])
// object History extends BotCombinators {
//   import fastparse.noApi._
//   import White._
//
//   /*****************************************************
//   * Repeat Last Immediate Command:
//   *   - <bot> !
//   *   - <bot> again
//   * Repeating the key phrase should repeat the action (ie !!! repeat twice)
//   *
//   * Repeat Last Command Starting With: (ie love)
//   *   - <bot> love!
//   *   - <bot> love again
//   *****************************************************/
//
//   val bang: P[Unit] = P(IgnoreCase("!"))
//   val again: P[Unit] = P(IgnoreCase("again"))
//   val opt: P[(Option[String],Option[String])] = P(bot ~ (bang|again).!.? ~ (AnyChar.rep).!.?)
//
//   def apply(imsg: IMessage): Option[History] = {
//     apply(imsg.content)
//   }
//
//   def apply(str: String): Option[History] = {
//     opt.parse(str) match {
//       case Parsed.Success(value, _) => Some(History(value, None))
//       case _ =>
//         println(opt.parse(str))
//         None
//     }
//   }
// }
