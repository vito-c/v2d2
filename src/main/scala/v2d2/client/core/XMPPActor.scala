package v2d2.client.core

import java.util.Collection

import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import org.jxmpp.jid.impl.JidCreate
import org.jxmpp.jid.Jid
import java.lang.System
import v2d2.actions.generic.HipUsers
import scala.language.postfixOps

import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
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
import v2d2.actions.generic.HipChatUsersAct
import v2d2.actions.generic.protocol._
import v2d2.actions.love._
import v2d2.actions.pager._
import v2d2.actions.who._
import v2d2.client._
import v2d2.mtg._
import v2d2.parsers._

case class XUserList(roster:List[RosterEntry])
class XMPPActor(connection: XMPPTCPConnection) 
extends Actor 
with ActorLogging {

  // import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  // implicit val timeout = Timeout(130.seconds)
  implicit val timeout = Timeout(25.seconds)

  val chatManager: ChatManager = ChatManager.getInstanceFor(connection)
  // private var _usersDirty: Boolean = true
  private var _usersCache: List[User] = Nil
  // private var _rosterDirty: Boolean = true
  // private var _rosterLoading = false
  // private var _rosterCache: List[RosterEntry] = Nil

  val searcher = context.actorOf(Props(classOf[UserSearchAct]), name = "usersearch")// + muc.getRoom() )
  val profile  = context.actorOf(Props(classOf[ProfileActor], connection), name = "cmd:profile" )
  val useraggregator = context.actorOf(Props(classOf[UserAggreator], connection), name = "cmd:useraggregator" )
  var counter  = System.currentTimeMillis()
  val hcnotifs = context.actorOf(Props(classOf[HipChatNotifs]), name = "hcnotifs")// + muc.getRoom() )
  val hcusers  = context.actorOf(Props(classOf[HipChatUsersAct]), name = "hcusers")// + muc.getRoom() )

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
    context.actorOf(Props(classOf[MagicAct], None), name = "cmd:magic")// + muc.getRoom() )
    // context.actorOf(Props(classOf[ServerAct], muc), name = "server")// + muc.getRoom() )
    //
    // context.actorOf(Props(classOf[HistoryAct], muc), name = "history")// + muc.getRoom() )
    // _rosterLoading = true
    // _rosterLoading = false
    // _rosterDirty = false

    // adding this fails at life

    val chatmanager = ChatManager.getInstanceFor(connection);

    chatManager.addChatListener(
      new ChatManagerListener() {
        @Override
        def chatCreated(chat: Chat, createdLocally: Boolean) {
          log.info("CHAT CREATED")
          chat.addMessageListener(new ChatMessageListener() {
            def processMessage(chat: Chat, message: Message) {
              val imsg: IMessage = new XMessage(message)
              val sender = XmppStringUtils.parseResource(message.getFrom().toString())
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
      counter = System.currentTimeMillis()
      ChatManager.getInstanceFor(connection)
        .createChat(JidCreate.entityFrom(imsg.fromJid)).sendMessage(response)

    case Relay(imsg) =>
      log.info(s"CHILDREN: ${context.children}")
      if(imsg == null || imsg.content == null || imsg.content == "") None
      else context.children foreach { child => 
        if(child.path.name.matches("cmd:.*"))
          child ! imsg 
      }

    // This is a test remove it when done
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
        .createChat(JidCreate.entityFrom(xhr.originalMsg.fromJid)).sendMessage(msg)

    case h: HipUsersReq =>
      hcusers forward h

    case p: Ping =>
      if (System.currentTimeMillis() - counter > 5000) {
        counter = System.currentTimeMillis()
        PingManager.getInstanceFor(connection).pingMyServer()
      }

    case f:FindUser => 
      searcher forward f

    case UserList() =>
      log.info("IN USER LIST")
      useraggregator ! UserList()

    case UserListResponse(users) =>
      log.info(s"users head: ${users.head}")
      _usersCache = users map { u => u }
      log.info(s"users head: ${_usersCache.head}")


    // case f:FindUser =>
    //   log.info(s"find: ${f} and ${f.search}")
    //   (for {
    //     sa <- f.search
    //   } yield(sa)).collect({
    //     case e: Email =>
    //       log.info(s"email: ${e}")
    //       (for {
    //         emap <- (self ? EmailMap()).mapTo[Map[String,User]]
    //       } yield(emap.get(e.needle))) pipeTo sender
    //     case n: Nick =>
    //       log.info(s"nick: ${n}")
    //       (for {
    //         nmap <- (self ? NickMap()).mapTo[Map[String,User]]
    //       } yield(nmap.get(n.needle))) pipeTo sender
    //     case _ =>
    //       log.info(s"find: ${f} and ${f.search} not found")
    //   })
    //
      // f.search match {
      //   case e: Email =>
      //     log.info(s"email: ${e}")
      //     val c = for {
      //       emap <- (self ? EmailMap()).mapTo[Map[String,User]]
      //       // user <- (emap.get(email)).asInstanceOf[Option[User]]
      //     } yield(
      //       emap.get(e.needle)) 
      //     c.future pipeTo sender
      //   case _ =>
      //     log.info(s"find: ${f} and ${f.search} not found")
      //    
      //   // case n:Nick =>
      //   // case f:FullName =>
      //   // case u:UName =>
      //   // case n:Name =>
      // }
      //   val con = for {
      //     emap <- (self ? EmailMap()).mapTo[Map[String,User]]
      //     // user <- (emap.get(email)).asInstanceOf[Option[User]]
      //   } yield(
      //     // log.info(s"\n\temail: $email \n\tuser: ${emap.get(email)}")
      //     emap.get(email)
      //     // match {
      //     //   case Some(user) => 
      //     //     pprint.log(user, "user")
      //     //     sender ! user
      //     //   case _ => 
      //     //     log.info("FAILED")
      //     // }
      //   ) //pipeTo sender
      //   con pipeTo sender


    // case rq: ProfileRQ =>
    //   val pp = Promise[Profile]()
    //   val f = Future {
    //     counter = System.currentTimeMillis()
    //     connection.sendIqWithResponseCallback(ProfileIQ(rq.jid), new StanzaListener() {
    //       def processStanza(packet: Stanza) = {
    //         if (packet != null && packet.isInstanceOf[ProfileIQ]) {
    //           pp.success(
    //             Profile(packet.asInstanceOf[ProfileIQ])
    //           )
    //         } else {
    //           pprint.log(rq, "Failure")
    //           pp.failure(
    //             throw new Exception(s"failed ${rq.jid}"))
    //         }
    //       }
    //     })
    //   } 
    //   pp.future pipeTo sender

    // case entry: RosterEntry =>
    //   val req = for {
    //     p <- (profile ? ProfileRQ(entry.getUser())).mapTo[Profile]
    //   } yield { 
	// 	User(
    //       name     = entry.getName(),
    //       jid      = entry.getUser(),
    //       nick     = p.mention_name,
    //       email    = p.email,
    //       timezone = Timezone(p.timezone, p.offset),
    //       entry    = entry
    //     ) 
    //   } 
    //   req pipeTo sender

    // case RosterList() =>
    //   log.info("roster list request")
    //   Future {
    //     if( _rosterDirty == false ) {
    //       log.info("CACHED ROSTER")
    //       _rosterCache
    //     } else {
    //       _roster.reloadAndWait()
    //       _rosterDirty = false
    //       _rosterCache = _roster.getEntries().asScala.toList
    //       _rosterCache //needs to be here
    //     }
    //   } pipeTo sender

    // case UserList() =>
    //   log.info("user list request")
    //   val req = for {
    //     roster <- (self ? RosterList()).mapTo[List[RosterEntry]]
    //   } yield {
    //     roster
    //   }
    //   req onComplete {
    //     case Success(result)  =>
    //         Future.sequence(
    //           roster map { re =>
    //             for { user <- (self ? re).mapTo[User] } yield {
    //               // log.info(s"inside future map ${user.name}")
    //               user
    //             }
    //         })
    //       log.info("============================================")
    //       log.info(s"HELLO IT HAPPENED ${result}")
    //     case Failure(t) =>
    //       log.info("User list ERROR")
    //       context.parent ! "An error has occured: " + t.getMessage()
    //   }

    // case XUserList(roster) =>
    //   val num = 768
    //   log.info(s"ROSTER ENTRIES ${roster.length}")
    //   log.info(s"ROSTER ENTRIES ${roster(num)}")
    //   pprint.log(roster(num), "roster")
    //   val out: Future[List[User]] =         u1
    //     // val u2 = Future.sequence {
    //     //   roster.slice(760,roster.length) map { re =>
    //     //     for {user <- (self ? re).mapTo[User]} yield {
    //     //       user
    //     //     }
    //     //   }
    //     // }
    //     // for{
    //     //   f1Res <- u1
    //     //   f2Res <- u2
    //     // } yield (f1Res ::: f2Res)
    //   } else {
    //     log.info("USERS CACHED")
    //     Future { _usersCache }
    //   }
    //   out onComplete {
    //     case Success(result: List[User])  =>
    //       log.info(s"SUCCESS")
    //     case Failure(t: Throwable) =>
    //       log.info("ERROR")
    //       context.parent ! s"An error has roster occurred: ${t.getMessage()}"
    //   }
    //   out pipeTo sender

    // case UserList() =>
    //   log.info("user list request")
    //   val req = for {
    //     roster <- (self ? RosterList()).mapTo[List[RosterEntry]]
    //     userlist <- 
    //       if(_usersDirty == true) {
    //         log.info("fresh users to map")
    //         _usersDirty = false
    //         counter = System.currentTimeMillis()
    //         Future.sequence {
    //           val len = roster.length
    //           var i = 1
    //           while(100*i < len) {
    //             roster.take(100*i) map { re =>
    //               Thread.sleep(100)
    //               for { user <- (self ? re).mapTo[User] } yield {
    //                 user
    //               }
    //             }
    //             i += 1
    //             Thread.sleep(5000) //550
    //           }
    //           roster.slice(100*i-100,len) map { re =>
    //             for { user <- (self ? re).mapTo[User] } yield {
    //               user
    //             }
    //           }
    //           // val len = roster.length
    //           // val s = len/3
    //           // val m = len - len/3
    //           // val tip = roster.take(len/3) map { re =>
    //           //   for {user <- (self ? re).mapTo[User]} yield {
    //           //     user
    //           //   }
    //           // }
    //           //
    //           // log.info(s"TIP DONE ${tip.length}")
    //           // val mid = roster.slice(len/3,2*len/3) map { re =>
    //           //   for {user <- (self ? re).mapTo[User]} yield {
    //           //     user
    //           //   }
    //           // }
    //           //
    //           // val num = 2*len/3 + 318
    //           // val delta = len-num
    //           // log.info(s"MID DONE ${mid.length}")
    //           // log.info(s"len: ${len} num: ${num} delta: ${len-num}")
    //           // val end = roster.slice(2*len/3,num) map { re =>
    //           //   for {user <- (self ? re).mapTo[User]} yield {
    //           //     user
    //           //   }
    //           // }
    //           //
    //           // log.info(s"END DONE ${end.length}")
    //           //
    //           // val end2 = roster.slice(num,num +delta/4) map { re =>
    //           //   pprint.log(re,"roster entry")
    //           //   for {user <- (self ? re).mapTo[User]} yield {
    //           //     println(s"nick: ${user.nick} ${re.getName()}")
    //           //     user
    //           //   }
    //           // }
    //           //
    //           // log.info(s"END DONE 2 ${end2.length}")
    //           //
    //           // val end3 = roster.slice(num + delta/4,num + delta/4) map { re =>
    //           //   pprint.log(re,"roster entry")
    //           //   for {user <- (self ? re).mapTo[User]} yield {
    //           //     println(s"nick: ${user.nick} ${re.getName()}")
    //           //     user
    //           //   }
    //           // }
    //           // log.info(s"END DONE 3 ${end3.length}")
    //           // Thread.sleep(5000)
    //           // tip ++ mid ++ end ++ end2 //++ end3
    //         }
    //       } else {
    //           log.info("USERS CACHED")
    //           Future { _usersCache }
    //       }
    //   } yield { 
    //     _usersCache = userlist
    //     userlist
    //   }
    //   req pipeTo sender

      //   log.info("in the user list yield")
      //   userlist
      // }) pipeTo sender
      // req onComplete {
      //   case Success(result)  =>
      //     log.info("USER LIST COMPLETE")
      //     _usersCache = result
      //     _usersDirty = false
      //   case Failure(t) =>
      //     context.parent ! "An error has occured: " + t.getMessage
      // }
      // req pipeTo sender

      // case FindUser(Some(email), _, _, _) =>
      //   log.info(s"email: $email")
      //   val con = for {
      //     emap <- (self ? EmailMap()).mapTo[Map[String,User]]
      //     // user <- (emap.get(email)).asInstanceOf[Option[User]]
      //   } yield(
      //     // log.info(s"\n\temail: $email \n\tuser: ${emap.get(email)}")
      //     emap.get(email)
      //     // match {
      //     //   case Some(user) => 
      //     //     pprint.log(user, "user")
      //     //     sender ! user
      //     //   case _ => 
      //     //     log.info("FAILED")
      //     // }
      //   ) //pipeTo sender
      //   con pipeTo sender

        // con onComplete {
        //   case Success(emap) =>
        //     sender ! emap.get(email)
        //   case Failure(t) =>
        //     log.info(s"FAILED: ${t.getMessage}")
        // }
        //
      // case FindUser(_, Some(jid), _, _) => None
      //   for {
      //     jmap <- (self ? UserMap()).mapTo[Map[String,User]]
      //   } yield(
      //     jmap get (jid) match {
      //       case Some(user) => sender ! user
      //       case _ => sender ! None
      //     }
      //   )

      // case FindUser(_, _, Some(name), _) => None

      // case FindUser(_, _, _, Some(nick)) => None
      //   for {
      //     nmap <- (self ? NickMap()).mapTo[Map[String,User]]
      //   } yield(
      //     nmap get (nick) match {
      //       case Some(user) => sender ! user
      //       case _ => sender ! None
      //     }
      //   )
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
      // Future { 
      //   log.info(s"user map request ${_usersCache.length}")
      //   _usersCache.map(u => u.jid -> u).toMap 
      // } pipeTo sender
      val out = _usersCache.map(u => u.jid -> u).toMap
      pprint.log(out)
      sender() ! _usersCache.map(u => u.jid -> u).toMap
      

    case EmailMap() =>
      log.info("email map request")
      val out = _usersCache.map(u => u.nick -> u).toMap
      log.info(s"out len ${out.size}")
      pprint.log(out)
      sender() ! _usersCache.map(u => u.email -> u).toMap
      // (for {
      //   ulist <- (self ? UserList()).mapTo[List[User]]
      // } yield {
      //   ulist.map(u => u.email -> u).toMap
      // }) pipeTo sender

    case NickMap() =>
      // Future { 
      //   log.info(s"nick map request ${_usersCache.length}")
      //   _usersCache.map(u => u.nick -> u).toMap 
      // } pipeTo sender
      log.info(s"nick map request ${_usersCache.length}")
      val out = _usersCache.map(u => u.nick -> u).toMap
      log.info(s"out len ${out.size}")
      pprint.log(out)
      sender() ! _usersCache.map(u => u.nick -> u).toMap
      

    case NameMap() =>
      sender() ! _usersCache.map(u => u.name -> u).toMap 

    case JoinRoom(room, chatpass) =>
      log.info("joining room")
      // val rname = room.replaceAll("@.*$","")
      val muc = MultiUserChatManager
        .getInstanceFor(connection)
        .getMultiUserChat(JidCreate.entityBareFrom(room))
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
