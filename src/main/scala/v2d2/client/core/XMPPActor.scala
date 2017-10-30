package v2d2.client.core

import java.util.Collection

import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.stream.ActorMaterializer
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.util.Timeout
import org.jivesoftware.smack.StanzaListener
import org.jivesoftware.smack.chat.{Chat, ChatManager, ChatManagerListener, ChatMessageListener}
import org.jivesoftware.smack.packet.{Message, Presence, Stanza}
import org.jivesoftware.smack.roster.{Roster, RosterEntry, RosterListener}
import org.jivesoftware.smack.tcp.XMPPTCPConnection
import org.jivesoftware.smackx.muc.MultiUserChatManager
import org.jivesoftware.smackx.ping.PingManager
import org.jivesoftware.smackx.xhtmlim.XHTMLManager
import org.jxmpp.util.XmppStringUtils
import v2d2.V2D2
import v2d2.actions.generic._
import v2d2.actions.generic.protocol._
import v2d2.actions.love._
import v2d2.actions.pager._
import v2d2.mtg.MagicAct
import v2d2.actions.who._
import v2d2.client._
import v2d2.mtg._
import java.io.InputStream

class XMPPActor(connection: XMPPTCPConnection) 
extends Actor 
with ActorLogging 
with CardSetProtocol {

  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25.seconds)

  val chatManager: ChatManager = ChatManager.getInstanceFor(connection)
  val _roster:Roster = Roster.getInstanceFor(connection)
  private var _usersDirty: Boolean = true
  private var _usersCache: List[User] = Nil
  private var _rosterDirty: Boolean = true
  private var _rosterLoading = false
  private var _rosterCache: List[RosterEntry] = Nil


  override def preStart = {

    context.actorOf(Props(classOf[Quitter]), name = "cmd:quit")// + muc.getRoom() )
    context.actorOf(Props(classOf[NailedIt]), name = "cmd:nailed")// + muc.getRoom() )
    context.actorOf(Props(classOf[Help]), name = "cmd:help")// + muc.getRoom() )
    // context.actorOf(Props(classOf[Knocker], muc), name = "knocker")// + muc.getRoom() )
    // context.actorOf(Props(classOf[LoveAct], muc), name = "cmd:lover")// + muc.getRoom() )
    context.actorOf(Props(classOf[WhoLoveAct]), name = "cmd:wholove")// + muc.getRoom() )
    context.actorOf(Props(classOf[WhoAct]), name = "cmd:whois")// + muc.getRoom() )
    context.actorOf(Props(classOf[PagerAct]), name = "cmd:pager")// + muc.getRoom() )
    context.actorOf(Props(classOf[ServerAct]), name = "cmd:server")// + muc.getRoom() )
    context.actorOf(Props(classOf[MagicAct]), name = "cmd:magic")// + muc.getRoom() )
    // context.actorOf(Props(classOf[ServerAct], muc), name = "server")// + muc.getRoom() )
    // context.actorOf(Props(classOf[HistoryAct], muc), name = "history")// + muc.getRoom() )
    // _rosterLoading = true
    // _roster.reloadAndWait()
    // _rosterCache = _roster.getEntries().asScala.toList
    // _rosterLoading = false
    // _rosterDirty = false

    // adding this fails at life
    // _roster.setSubscriptionMode(Roster.SubscriptionMode.accept_all)
    _roster.addRosterListener(new RosterListener(){
      def entriesAdded(args: Collection[String]) = {
        log.info("entires added")
        // _rosterDirty = true
        // _mapsDirty = true
        // TBD
      }
      def entriesDeleted(args: Collection[String]) = {
        log.info("entires deleted")
        // _rosterDirty = true
        // _mapsDirty = true
        // TBD
      }
      def entriesUpdated(args: Collection[String]) = {
        log.info("entires updated")
        // _rosterDirty = true
        // _mapsDirty = true
        // TBD
      }
      def presenceChanged(args: Presence) = {
        // log.info(s"presence changed")
        // _rosterDirty = true
        // _mapsDirty = true
        // TBD
      }
    })

    val chatmanager = ChatManager.getInstanceFor(connection);

    chatManager.addChatListener(
      new ChatManagerListener() {
        @Override
        def chatCreated(chat: Chat, createdLocally: Boolean) {
          log.info("CHAT CREATED")
          chat.addMessageListener(new ChatMessageListener() {
            def processMessage(chat: Chat, message: Message) {
              val imsg: IMessage = new XMessage(message)
              val sender = XmppStringUtils.parseResource(message.getFrom())
              log.info("RECEIVED MESSAGE: " + message.getBody());
              if(imsg != null && imsg.content != null && sender != V2D2.display) {
                self ! Relay(imsg)
              }
            }
          });
        }
    })
  }

  def receive: Receive = {

    case Response(imsg, response) =>
      ChatManager.getInstanceFor(connection)
        .createChat(imsg.fromJid).sendMessage(response)

    case Relay(imsg) =>
      log.info(s"CHILDREN: ${context.children}")
      if(imsg == null || imsg.content == null || imsg.content == "") None
      else context.children foreach { child => 
        if(child.path.name.matches("cmd:.*"))
          child ! imsg 
      }

    case xhr: XHTMLResponse =>
      val xh = xhr.response
      val msg = new Message()
      msg.setBody("&lt;pre&gt;test notif&lt;/pre&gt;")
      // msg.setBody("this is a test")
      val xhtmlBody = xh.dump()
      // self ! s"html ${xhtmlBody}"
      // Add the XHTML text to the message
      // msg.addExtension(xh.notif())
      XHTMLManager.addBody(msg, xhtmlBody);
// <x xmlns='http://hipchat.com/protocol/muc#room'>
// <message_format>html</message_format><color>purple</color><type>system</type><notify>0</notify></x>
      log.info(s"${msg}")
      // Send the message that contains the XHTML
      // self ! s"msg ${msg}"
      ChatManager.getInstanceFor(connection)
        .createChat(xhr.originalMsg.fromJid).sendMessage(msg)

    case p: Ping =>
	  PingManager.getInstanceFor(connection).pingMyServer();

    case MakeRosterDirty() =>
      log.info("dirty roster")
      _rosterDirty = true;

    case rq: ProfileRQ =>
      val pp = Promise[Profile]()
      Future {
        connection.sendIqWithResponseCallback(ProfileIQ(rq.jid), new StanzaListener() {
          def processPacket(packet: Stanza) = {
            if (packet != null && packet.isInstanceOf[ProfileIQ]) {
              pp.success(Profile(packet.asInstanceOf[ProfileIQ]))
            } else {
              pp.failure(//UserUseless(s"Failed: ${rq.jid}"))
                throw new Exception(s"failed ${rq.jid}"))
            }
          }
        })
      }
      pp.future pipeTo sender

    case entry: RosterEntry => //return a user
      val req = for {
        profile <- (self ? ProfileRQ(entry.getUser())).mapTo[Profile]
      } yield( User(
          name     = entry.getName(),
          jid      = entry.getUser(),
          nick     = profile.mention_name,
          email    = profile.email,
          timezone = Timezone(profile.timezone, profile.offset),
          entry    = entry
      ) )
      req pipeTo sender

    case RosterList() =>
      log.info("roster list request")
      Future {
        if( _rosterDirty == false ) {
          log.info("CACHED ROSTER")
          _rosterCache
        } else {
          _roster.reloadAndWait()
          _rosterDirty = false
          _rosterCache = _roster.getEntries().asScala.toList
          _rosterCache //needs to be here
        }
      } pipeTo sender

    case UserList() =>
      log.info("user list request")
      val req = for {
        roster <- (self ? RosterList()).mapTo[List[RosterEntry]]
        userlist <-
          if(_usersDirty == true) {
            log.info("fresh users to map")
            Future.sequence(
              roster map { re =>
                for { user <- (self ? re).mapTo[User] } yield(user)
            })
          } else {
            log.info("USERS CACHED")
            Future { _usersCache }
          }
      } yield(
        userlist
      )
      req pipeTo sender
      req onComplete {
        case Success(result)  =>
          log.info("USER LIST COMPLETE")
          _usersCache = result
          _usersDirty = false
        case Failure(t) =>
          context.parent ! "An error has occured: " + t.getMessage
      }

      case FindUser(Some(email), _, _, _) =>
        log.info(s"email: $email")
        val con = for {
          emap <- (self ? EmailMap()).mapTo[Map[String,User]]
          // user <- (emap.get(email)).asInstanceOf[Option[User]]
        } yield(
          // log.info(s"\n\temail: $email \n\tuser: ${emap.get(email)}")
          emap.get(email)
          // match {
          //   case Some(user) => 
          //     pprint.log(user, "user")
          //     sender ! user
          //   case _ => 
          //     log.info("FAILED")
          // }
        ) //pipeTo sender
        con pipeTo sender

        // con onComplete {
        //   case Success(emap) =>
        //     sender ! emap.get(email)
        //   case Failure(t) =>
        //     log.info(s"FAILED: ${t.getMessage}")
        // }
        //
      case FindUser(_, Some(jid), _, _) => None
        for {
          jmap <- (self ? UserMap()).mapTo[Map[String,User]]
        } yield(
          jmap get (jid) match {
            case Some(user) => sender ! user
            case _ => sender ! None
          }
        )

      case FindUser(_, _, Some(name), _) => None

      case FindUser(_, _, _, Some(nick)) => None
        for {
          nmap <- (self ? NickMap()).mapTo[Map[String,User]]
        } yield(
          nmap get (nick) match {
            case Some(user) => sender ! user
            case _ => sender ! None
          }
        )
    // case GetUserByJid(jid) =>
    //   for {
    //     jmap <- (self ? UserMap()).mapTo[Map[String,User]]
    //   } yield(
    //     jmap get (jid) match {
    //       case Some(user) => sender ! user
    //       case _ => sender ! None
    //     }
    //   )
    // case GetUserByNick(nick) =>
    //   for {
    //     nmap <- (self ? NickMap()).mapTo[Map[String,User]]
    //   } yield(
    //   )
    // case GetUserByEmail(email) =>
    //   for {
    //     emap <- (self ? EmailMap()).mapTo[Map[String,User]]
    //   } yield(
    //   )

    case UserMap() =>
      log.info("user map request")
      val req = for {
        ulist <- (self ? UserList()).mapTo[List[User]]
      } yield(ulist.map(u => u.jid -> u).toMap)
      req pipeTo sender

    case EmailMap() =>
      log.info("nick map request")
      val req = for {
        ulist <- (self ? UserList()).mapTo[List[User]]
      } yield(ulist.map(u => u.email -> u).toMap)
      req pipeTo sender

    case NickMap() =>
      log.info("nick map request")
      val req = for {
        ulist <- (self ? UserList()).mapTo[List[User]]
      } yield(ulist.map(u => u.nick -> u).toMap)
      req pipeTo sender

    case JoinRoom(room, chatpass) =>
      log.info("joining room")
      // val rname = room.replaceAll("@.*$","")
      val muc = MultiUserChatManager
        .getInstanceFor(connection)
        .getMultiUserChat(room)
      val mactor = context.actorOf(
        Props(classOf[MUCActor], muc, connection),
        name = room
      )
      // mactor ! "Hello, humans"

    case _ => None
  }

  override def postStop() = {
    if (connection.isConnected) connection.disconnect()
    log.info("SHUT DOWN")
    context.system.terminate()
  }
}
