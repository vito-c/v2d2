package v2d2.client.core

import scala.collection.immutable.Queue
import akka.actor.{Actor, ActorContext, Props, ActorLogging}
import v2d2.actions.knock.Knocker
import org.jivesoftware.smack.StanzaListener
import org.jivesoftware.smack.packet.Stanza
import org.jivesoftware.smack.packet.{Presence, Message}
import org.jivesoftware.smack.tcp.{XMPPTCPConnectionConfiguration, XMPPTCPConnection}
import org.jivesoftware.smack.{MessageListener,PresenceListener}
import org.jivesoftware.smackx.muc.{MultiUserChatManager, DiscussionHistory, MultiUserChat}
import org.jxmpp.util.XmppStringUtils
import v2d2.V2D2
import v2d2.client.{IMessage, XMessage, MUMessage, XHTMLMemo}
import v2d2.client.{Profile,ProfileIQ}
import v2d2.actions.generic.protocol._
import v2d2.actions.generic._
import org.jivesoftware.smackx.xhtmlim.XHTMLManager

class MUCActor(muc: MultiUserChat, connection: XMPPTCPConnection) extends Actor with ActorLogging {

  // non breaking space: \u2002
  override def preStart = {
    val history = new DiscussionHistory()
    history.setMaxChars(0) // Don't get anything when joining
    history.setMaxStanzas(0)
    muc.join(V2D2.display, "", history, 5000)
    context.actorOf(Props(classOf[Quitter]), name = "quit")// + muc.getRoom() )
    context.actorOf(Props(classOf[NailedIt]), name = "nailed")// + muc.getRoom() )
    context.actorOf(Props(classOf[Help]), name = "help")// + muc.getRoom() )
    context.actorOf(Props(classOf[Knocker], muc), name = "knocker")// + muc.getRoom() )
    context.actorOf(Props(classOf[Lover], muc), name = "lover")// + muc.getRoom() )
    context.actorOf(Props(classOf[ServerAct], muc), name = "server")// + muc.getRoom() )
    context.actorOf(Props(classOf[HistoryAct], muc), name = "history")// + muc.getRoom() )

    muc.addParticipantListener(new PresenceListener() {
      override def processPresence(presence: Presence) = {
        log.info(s"presence has changed in chat make roster dirty\n\t${presence}")
        // Leave this as a reminder:
        // you can't context.actorSelection("/user/xmpp") ! MakeRosterDirty()
        // or even context.parent inside the eventlisteners b/c it's not thread safe
        // so you have to send a message to yourself and then handle it.
        self ! MakeRosterDirty()
        log.info(s"presence updated")
      }
    })
    muc.addMessageListener(new MessageListener() {
      def processMessage(msg: Message) = {
        val sender = XmppStringUtils.parseResource(msg.getFrom())
        if (sender != V2D2.display) {
          val imsg: IMessage = new MUMessage(msg, muc)
          log.info(s"process msg" +
            s"\n\tsender: ${msg.getFrom()}" +
            s"\n\tcontent: ${msg.getBody()}" +
            s"\n\txml: ${msg.toXML()}" +
            s"\n\tdisplay: ${V2D2.display}")

          if(imsg != null && imsg.content != null)
            self ! Relay(imsg)
          else
            log.info("dont send")
        }
      }
    })
  }

  override def postStop() = {
    muc.leave()
  }

  private var _history: Queue[IMessage] = Queue[IMessage]()
  private val _maxHist: Int = 200

  def receive: Receive = {

    case MakeRosterDirty() =>
      context.parent ! MakeRosterDirty()

    case Relay(imsg) =>
      if(imsg == null || imsg.content == null || imsg.content == "") None
      else context.children foreach { child => child ! imsg }
      if(imsg != null && imsg.fromJid != "" && (imsg.fromJid != V2D2.v2d2Jid))
        _history = _history.enqueue(imsg)
      if(_history.length > _maxHist)
        _history = _history.dequeue._2

    case h:History =>
      log.info(s"send history ${_history}")
      log.info(s"send history ${h.again}")
      sender ! History(Some(_history), h.again)

    case str: String =>
      muc.sendMessage(str)
    case xh: XHTMLMemo =>
      val msg = new Message()
      // msg.setBody("hello world")
      msg.setBody("hello world")
      val xhtmlBody = xh.dump()
      // self ! s"html ${xhtmlBody}"
      // Add the XHTML text to the message
      // msg.addExtension(xh.notif())
      XHTMLManager.addBody(msg, xhtmlBody);
      log.info(s"${msg}")
      // Send the message that contains the XHTML
      // self ! s"msg ${msg}"
      muc.sendMessage(msg);
    case a:Any =>
      self ! s"sorry dave I just can't do that ${a}"
    case _ =>
      self ! s"what you talking about willis"
  }
}
