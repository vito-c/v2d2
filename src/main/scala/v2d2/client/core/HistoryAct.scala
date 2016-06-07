package v2d2.client.core

import akka.actor.{Actor, ActorContext, Props, ActorLogging}
import v2d2.client.{IMessage,DMessage,MsgData}
import org.jivesoftware.smackx.muc.MultiUserChat
import v2d2.actions.generic.protocol._ 

class HistoryAct(muc: MultiUserChat) extends Actor with ActorLogging {
  def receive: Receive = {
    case imsg: IMessage =>
      Again(imsg) match {
        case Some(again) => 
          log.info(s"again ${again}")
          context.parent ! History(None, Some(again))
        case _ => None
      }

    case history: History =>
      val msgs = history.msgs.get
      val again = history.again.get
      again.cmd match {
        case Some(cmd) =>
          // brute force right now
          cmd.toLowerCase match {
            case "love" | "l" | "lov" => 
              val loveIdx = msgs.lastIndexWhere { msg => Love(msg) != None }
              val ms = msgs(loveIdx)
              val am = again.imsg
              val dm = new DMessage(MsgData(
                ms.content, am.fromJid, am.fromName, am.fromNick, am.fromRaw, am.fromMsgJid))
              context.parent ! Relay(dm)
            case "nail" | "n" =>
              log.info(s"running nailed ${Nailed("nailed")}")
              context.actorSelection("../nailed") ! Nailed("nailed").get
            case "help" | "h" =>
              log.info(s"running help ${Helpme("helpme")}")
              log.info("running help")
              context.actorSelection("../help") ! Helpme("helpme").get
            case _ => 
              log.warning("WARN WARN")
              None
              // context.parent ! s"Sorry I don't know about cmd ${cmd}"
          }
        case _ =>
          log.info("running repeat last")
          // brute force right now
          val loveIdx = msgs.lastIndexWhere { msg => Love(msg) != None }
          val nailedIdx = msgs.lastIndexWhere { msg => Nailed(msg) != None }
          val helpmeIdx = msgs.lastIndexWhere { msg => Helpme(msg) != None }
          val nl = List(loveIdx, nailedIdx, helpmeIdx).max
          val ms = msgs(nl)
          val am = again.imsg
          val dm = new DMessage(MsgData(
            ms.content, am.fromJid, am.fromName, am.fromNick, am.fromRaw, am.fromMsgJid))
          context.parent ! Relay(dm)
      }

  }
}
