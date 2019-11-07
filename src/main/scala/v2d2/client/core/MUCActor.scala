package v2d2.client.core

import akka.actor.{Actor, ActorContext, Props, ActorLogging}
import java.lang.System
import java.util.Collection
import org.jivesoftware.smack.StanzaListener
import org.jivesoftware.smack.packet.DefaultExtensionElement
import org.jivesoftware.smack.packet.Stanza
import org.jivesoftware.smack.packet.{Presence, Message}
import org.jivesoftware.smack.tcp.{XMPPTCPConnectionConfiguration, XMPPTCPConnection}
import org.jivesoftware.smack.{MessageListener,PresenceListener}
import org.jivesoftware.smackx.muc.{MultiUserChatManager, DiscussionHistory, MultiUserChat}
import org.jivesoftware.smackx.xhtmlim.XHTMLManager
import org.jxmpp.jid.parts.Resourcepart
import org.jxmpp.util.XmppStringUtils
import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.collection.immutable.Queue
import v2d2.V2D2
import v2d2.actions.generic._
import v2d2.actions.generic.protocol._
import v2d2.actions.knock.Knocker
import v2d2.actions.love._
import v2d2.actions.pager._
import v2d2.actions.who._
import v2d2.client.{IMessage, XMessage, MUMessage, XHTMLMemo,XHTMLResponse}
import v2d2.client.{Profile,ProfileIQ}
import v2d2.mtg.MagicAct
import v2d2.mtg._
import v2d2.parsers.FindUser

class MUCActor(muc: MultiUserChat, connection: XMPPTCPConnection) extends Actor with ActorLogging {

  val hcnotifs = context.actorOf(Props(classOf[HipChatNotifs]), name = "hcnotifs")// + muc.getRoom() )
  val searcher = context.actorOf(Props(classOf[UserSearchAct]), name = "usersearch")// + muc.getRoom() )
  val hcusers  = context.actorOf(Props(classOf[HipChatUsersAct]), name = "hcusers")
  val history = new DiscussionHistory()
  // lazy val rosterAct = context.actorOf(Props(classOf[RosterActor], connection), name = "roster") //! RosterList()
  // non breaking space: \u2002
  // override def preStart = {
    // DEBUG GET USERS IN ROOM:
    // val jl = muc.getParticipants() map { o => 
    //   o.getJid()
    // }
    // pprint.log(jl, "list")
    // val rosterAct = context.actorSelection("akka://system/user/xmpp/useraggregator/roster")
    history.setMaxChars(0) // Don't get anything when joining
    history.setMaxStanzas(0)
    muc.join(Resourcepart.from(V2D2.display.toString()), "", history, 5000)
    context.actorOf(Props(classOf[Quitter]), name = "quit")// + muc.getRoom() )
    context.actorOf(Props(classOf[NailedIt]), name = "nailed")// + muc.getRoom() )
    context.actorOf(Props(classOf[Help]), name = "help")// + muc.getRoom() )
    context.actorOf(Props(classOf[Knocker], muc), name = "knocker")// + muc.getRoom() )
    context.actorOf(Props(classOf[PagerAct]), name = "pager")// + muc.getRoom() )
    context.actorOf(Props(classOf[LoveAct], muc), name = "lover")// + muc.getRoom() )
    context.actorOf(Props(classOf[WhoLoveAct]), name = "wholove")// + muc.getRoom() )
    context.actorOf(Props(classOf[ServerAct]), name = "server")// + muc.getRoom() )
    context.actorOf(Props(classOf[HistoryAct], muc), name = "history")// + muc.getRoom() )
    context.actorOf(
      Props(classOf[WhoAct],
        Some(java.net.URLEncoder
            .encode(muc.getReservedNickname,"utf-8")
            .replace("+", "%20"))),
      name = "whois")
    context.actorOf(
      Props(classOf[MagicAct], 
      Some(
        java.net.URLEncoder.encode(muc.getReservedNickname,"utf-8")
          .replace("+", "%20"))), name = "magic")
    val o = muc.getOccupants().asScala.toList
    val p = o.map { j => 
      j.getResourceOrEmpty().toString()
    }.toList
    pprint.log(p, "GET THESE USERS")
    context.actorSelection("akka://v2d2/user/xmpp") ! Resources(p)

    muc.addParticipantListener(new PresenceListener() {
      override def processPresence(presence: Presence) = {
        // log.info(s"presence has changed in chat make roster dirty\n\t${presence}")
        // Leave this as a reminder:
        // you can't context.actorSelection("/user/xmpp") ! MakeRosterDirty()
        // or even context.parent inside the eventlisteners b/c it's not thread safe
        // so you have to send a message to yourself and then handle it.
        //
        // self ! MakeRosterDirty()
        // log.info(s"presence updated")
      }
    })
    muc.addMessageListener(new MessageListener() {
      def processMessage(msg: Message) = {
        val sender = XmppStringUtils.parseResource(msg.getFrom().toString())
        if (sender != V2D2.display && msg.getBody() != null) {
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
  // }

  // override def postStop() = {
  //   muc.leave()
  // }

  private var _history: Queue[IMessage] = Queue[IMessage]()
  private val _maxHist: Int = 200

  def receive: Receive = {

    case RosterList(roster) =>
      pprint.log(roster, "LIST")

    case f:FindUser => 
      searcher forward f

    case MakeRosterDirty() =>
      context.parent ! MakeRosterDirty()

      // relay message to all actors
    case Relay(imsg) =>
      if(imsg == null || imsg.content == null || imsg.content == "") None
      else context.children foreach { child => child ! imsg }
      if(imsg != null && imsg.fromJid != "" && (imsg.fromJid != V2D2.v2d2Jid && imsg.content != null))
        _history = _history.enqueue(imsg)
      if(_history.length > _maxHist)
        _history = _history.dequeue._2

    case h:History =>
      log.info(s"send history ${_history}")
      log.info(s"send history ${h.again}")
      sender ! History(Some(_history), h.again)

    case h: HipNotif =>
      hcnotifs ! h

    case Response(imsg, response, Some(notif)) =>
      self ! notif

    case Response(imsg, response, None) =>
          muc.sendMessage(response) 

    case str: String =>
      muc.sendMessage(str)

    case xhr: XHTMLResponse =>
      val xh = xhr.response
      val msg = new Message()
      // msg.setBody("hello world")
      msg.setBody("&lt;pre&gt;test notif&lt;/pre&gt;")
      val hipHtml = new DefaultExtensionElement("x", "http://hipchat.com/protocol/muc#room")
        hipHtml.setValue("message_format", "html")
        hipHtml.setValue("color","purple")
        hipHtml.setValue("type","system")
        hipHtml.setValue("notify","0")
      msg.addExtension(hipHtml)
      val xhtmlBody = xh.dump()
      // self ! s"html ${xhtmlBody}"
      // Add the XHTML text to the message
      // msg.addExtension(xh.notif())
      XHTMLManager.addBody(msg, xhtmlBody);
      // Send the message that contains the XHTML
      // self ! s"msg ${msg}"
      log.info(s"BEFORE SENDING: ${msg}")
      muc.sendMessage(msg);
      
    case xh: XHTMLMemo =>
      val msg = new Message()
      // msg.setBody("hello world")
      msg.setBody("&lt;pre&gt;test notif&lt;/pre&gt;")
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
