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
import v2d2.client.{IMessage, XMessage, MUMessage}
import v2d2.client.{Profile,ProfileIQ}
import v2d2.actions.generic.protocol._
import v2d2.actions.generic._

// case class TestProfile(imsg:Option[String])
// object TestProfile extends AutoParser[TestProfile]
// class GetProfile extends Actor with ActorLogging {
//
//   def receive: Receive = {
//     case TestProfile(imsg) =>
//       context.parent ! ProfileReq(V2D2.vitoJid)
//     case imsg:IMessage =>
//       TestProfile(imsg) match {
//         case Some(cmd) =>
//           context.parent ! ProfileReq(V2D2.vitoJid)
//         case _ => None
//       }
//     case _ => None
//   }
// }

class MUCActor(muc: MultiUserChat, connection: XMPPTCPConnection) extends Actor with ActorLogging {

  // non breaking space: \u2002
  override def preStart = {
    val history = new DiscussionHistory()
    history.setMaxChars(0) // Don't get anything when joining
    history.setMaxStanzas(0)
    muc.join(V2D2.display, "", history, 5000)
    muc.addParticipantListener(new PresenceListener() {
      def processPresence(presence: Presence) = {
        log.info(s"presence has changed in chat make roster dirty")
        context.actorSelection("/user/xmpp") ! MakeRosterDirty()
      }
    })
    context.actorOf(Props(classOf[Quitter]), name = "quit")// + muc.getRoom() )
    context.actorOf(Props(classOf[NailedIt]), name = "nailed")// + muc.getRoom() )
    context.actorOf(Props(classOf[Help]), name = "help")// + muc.getRoom() )
    // context.actorOf(Props(classOf[GetProfile]), name = "getprofile" + muc.getRoom() )
    context.actorOf(Props(classOf[Knocker], muc), name = "knocker")// + muc.getRoom() )
    context.actorOf(Props(classOf[Lover], muc), name = "lover")// + muc.getRoom() )
    // context.actorOf(Props(classOf[History], muc), name = "history")// + muc.getRoom() )

    muc.addMessageListener(new MessageListener() {
      def processMessage(msg: Message) = {
        val sender = XmppStringUtils.parseResource(msg.getFrom())
        if (sender != V2D2.display) {
          val imsg: IMessage = new MUMessage(msg, muc)
          // log.info(s"process msg" +
          //   s"\n\tsender: ${msg.getFrom()}" +
          //   s"\n\tcontent: ${msg.getBody()}" +
          //   s"\n\txml: ${msg.toXML()}" +
          //   s"\n\tdisplay: ${V2D2.display}")
          // log.info(s"msg: ${imsg}")
          // log.info(s"content: ${imsg.content}")

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

    case Relay(imsg) =>
      log.info(s"imsg: ${imsg}")
      if(imsg == null || imsg.content == null || imsg.content == "") None
      else context.children foreach { child => child ! imsg }
      if(imsg != null && (imsg.fromJid != V2D2.v2d2Jid))
        _history = _history.enqueue(imsg)
      if(_history.length > _maxHist) 
        _history = _history.dequeue._2

    case ProfileReq(target) =>
      // val peeps = muc.getOccupants().asScala.toList
      // log.info(s"lis len: ${peeps.length}")
      // peeps map { peep =>
      //   val occupant = muc.getOccupant(peep)
      //   val entry = V2D2.roster().getEntry(occupant.getJid())
      //   log.info(s"===========================================" +
      //     s"\n\t${occupant.getNick()}" +
      //     s"\n\t${occupant.getJid()}" +
      //     s"\n\t${occupant.getAffiliation()}" +
      //     s"\n\t${entry.getName()}" +
      //     s"\n\t${entry.getStatus()}" +
      //     s"\n\t${entry.getType()}")
      //   log.info(s"===========================================")
      // }
      connection.sendIqWithResponseCallback(ProfileIQ(target),
        new StanzaListener() {
          def processPacket(packet: Stanza): Unit = {
            if (packet != null && packet.isInstanceOf[ProfileIQ]) {
              val profile = Profile(packet.asInstanceOf[ProfileIQ])
              self ! s"found a profile for @${profile.mention_name}"
            } else {
              if ( packet == null ) self ! "packet was null"
              else log.info(s"packet: \n\t${packet}")
              self ! s"sorry I could not find ${target}"
            }
          }
        }
      )
    case str: String =>
      muc.sendMessage(str)
    case _ =>
      self ! "sorry dave I just can't do that"
  }
}
