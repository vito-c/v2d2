package v2d2.client.core

import org.jxmpp.util.XmppStringUtils
import akka.actor.{ActorRef, Actor, ActorSystem, ActorContext, Props, ActorLogging}
import akka.http.scaladsl.model._
import akka.japi.Util.immutableSeq
import akka.pattern.{ask, pipe}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import collection.JavaConversions._
import concurrent.{Future, Promise}
import java.util.Collection
import v2d2.actions.knock.Knocker
import v2d2.actions.generic.protocol._
import v2d2.actions.generic._
import v2d2.actions.love._
import org.jivesoftware.smack.StanzaListener
import org.jivesoftware.smack.chat.{ChatMessageListener, ChatManager, ChatManagerListener, Chat}
import org.jivesoftware.smack.packet.{Stanza, Presence, Message}
import org.jivesoftware.smack.roster.{RosterListener,Roster,RosterEntry,RosterLoadedListener}
import org.jivesoftware.smack.tcp.{XMPPTCPConnectionConfiguration, XMPPTCPConnection}
import org.jivesoftware.smackx.muc.{MultiUserChatManager, MultiUserChat}
import org.jivesoftware.smackx.muc.{Affiliate, Occupant}
import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Success, Failure}
import v2d2.client.{Profile,ProfileIQ,User}
import org.jivesoftware.smack.{MessageListener,PresenceListener}
import v2d2.V2D2
import org.jivesoftware.smackx.ping.PingManager
import org.jivesoftware.smackx.xhtmlim.XHTMLManager
import v2d2.client._
import v2d2.actions.generic.protocol._

class XMPPActor(connection: XMPPTCPConnection) extends Actor with ActorLogging {

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

      case FindUser(Some(email), _, _, _) => None
        for {
          emap <- (self ? EmailMap()).mapTo[Map[String,User]]
        } yield(
          emap get (email) match {
            case Some(user) => sender ! user
            case _ => sender ! None
          }
        ) 

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
