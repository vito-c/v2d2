package v2d2.client.core

import akka.actor.{ActorRef, Actor, ActorSystem, ActorContext, Props, ActorLogging}
import org.jivesoftware.smack.tcp.{XMPPTCPConnectionConfiguration, XMPPTCPConnection}
import org.jivesoftware.smack.roster.{RosterListener,Roster,RosterEntry,RosterLoadedListener}
import org.jivesoftware.smackx.muc.{MultiUserChatManager, DiscussionHistory, MultiUserChat}
import org.jivesoftware.smack.chat.{ChatMessageListener, ChatManager, ChatManagerListener, Chat}
import org.jivesoftware.smack.packet.{Presence, Message}
import v2d2.client.Memo

class XMPPActor(connection: XMPPTCPConnection) extends Actor with ActorLogging {

  val chatManager: ChatManager = ChatManager.getInstanceFor(connection)

  override def preStart = {
    chatManager.addChatListener(
      new ChatManagerListener() {
        @Override
        def chatCreated(chat: Chat, createdLocally: Boolean)
        {
          if (!createdLocally) {
            chat.addMessageListener(new ChatMessageListener() {
              def processMessage(chat: Chat, msg: Message) = {
                chat.sendMessage("(shrug)")
                // val imsg:IMessage = new XMessage(msg)
                // log.info(s"process msg" +
                //   s"\n\tsender: ${msg.getFrom()}" +
                //   s"\n\tcontent: ${msg.getBody()}" +
                //   s"\n\txml: ${msg.toXML()}" +
                //   s"\n\tdisplay: ${V2D2.display}")
                // log.info(s"msg: ${imsg}")
                // log.info(s"content: ${imsg.content}")
                // if(imsg != null && imsg.content != null)
                //   self ! Relay(imsg)
                // else
                //   log.info("dont send")
              }
            }) // end of msg listener
          }
        }
    })

    // val _roster:Roster = Roster.getInstanceFor(connection)
    // var _rosterDirty:Boolean = true;
  }

  def receive: Receive = {
    // case NickMap() =>
    // case UserMap() =>
    // case UserList() =>
    //   val entries: List[RosterEntry] = {
    //     log.info("Waiting for roster to load")
    //     if( _rosterDirty ){
    //       _roster.reloadAndWait()
    //     }
    //     _roster.getEntries().asScala.toList
    //     _rosterDirty = false
    //     log.info("Roster Loaded")
    //   }
    //   entries map { entry =>
    //
    //     // user(entry)
    //   }
    // case entry:RosterEntry =>
    //   this ? Profile(entry.getUser())
    //   val pu = Promise[User]()
    //   Future{
    //     profile(entry.getUser()) onComplete {
    //       case Success(pr) =>
    //         pu.success(
    //           User(
    //             name     = entry.getName(),
    //             jid      = entry.getUser(),
    //             nick     = pr.mention_name,
    //             entry    = entry
    //           ))
    //       case Failure(t) =>
    //         log.error(s"failed with ${t}")
    //     }
    //   }
    //   pu.future
    // case RProfile(jid) =>
    //   val pp = Promise[Profile]()
    //   Future {
    //     connection.sendIqWithResponseCallback(ProfileIQ(jid), new StanzaListener() {
    //         def processPacket(packet: Stanza) = {
    //           if (packet != null && packet.isInstanceOf[ProfileIQ]) {
    //             pp.success(Profile(packet.asInstanceOf[ProfileIQ]))
    //           } else {
    //             pp.failure(UserUseless(s"Failed: ${jid}"))
    //           //   sender ! Profile(packet.asInstanceOf[ProfileIQ])
    //           // } else {
    //           //   log.error(UserUseless(s"Failed: ${jid}"))
    //           }
    //         }
    //       })
    //   }
    //   pp.future

    // case memo: Memo =>
    //   val msg: Message = new Message(memo.to, Message.Type.chat)
    //   val chatManager: ChatManager = ChatManager.getInstanceFor(connection)
    //   msg.setFrom(memo.from)
    //   chatmanager.createChat(memo.to, new ChatMessageListener() {
    //       def processMessage(chat: Chat, message: Message) {
    //         System.out.println("Received message: " + message);
    //       }
    //   }).sendMessage(memo.body)

    // case Relay(imsg) =>
    //   if(imsg == null || imsg.content == null || imsg.content == "") None
    //   else context.children foreach { child => child ! imsg }
    //
    case JoinRoom(room, chatpass) =>
      val muc = MultiUserChatManager
        .getInstanceFor(connection)
        .getMultiUserChat(room)

      val mactor = context.actorOf(
        Props(classOf[MUCActor], muc, connection),
        name = room
      )
      mactor ! "Hello, humans"
    case _ => None
  }

  override def postStop() = {
    if (connection.isConnected) connection.disconnect()
    log.info("SHUT DOWN")
    context.system.shutdown()
  }
}
