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
import akka.actor.{Actor, ActorContext, ActorLogging, ActorRef, ActorSystem, Props}
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
import v2d2.client.core._
import v2d2.client.User
import v2d2.mtg._
import v2d2.parsers._
import akka.contrib.pattern.Aggregator

case class TimedOut()
class UserAggreator(connection: XMPPTCPConnection) 
extends Actor 
with Aggregator
with ActorLogging {
  private var _rosterDirty: Boolean = true
  private var _rosterCache: List[RosterEntry] = Nil

  expectOnce {
    case UserList() => new ChainedAggregator(sender())
    case _ => context.stop(self)
  }

  class ChainedAggregator(originalSender: ActorRef) {

    import context.dispatcher

    private var hipChatUsers: List[HipUser] = Nil
    private var rosterEntries: List[RosterEntry] = Nil

    context.actorOf(Props(classOf[HipChatUsersAct])) ! GetAllHipUsers()
    expectOnce {
      case HipUsersResponse(users) => 
        log.info(s"HC USERS ${users.length}")
        hipChatUsers = users
        collectUsers()
    }

    context.actorOf(Props(classOf[RosterActor], connection)) ! RosterList()
    expectOnce {
      case RosterResponse(roster) => 
        log.info(s"ROSTER USERS ${roster.length}")
        rosterEntries = roster
        collectUsers()
    } 

    context.system.scheduler.scheduleOnce(300.second, self, TimedOut)
    expectOnce {
      case TimedOut =>
        log.info(s"TIMED OUT")
        collectUsers(force = true)
    }

    // hipChatUsers()
    // rosterEntries()
    //
    // def hipChatUsers() = {
  //   context.actorOf(Props(classOf[HipChatUsersAct])) ! HipUsersReq()
    //   expectOnce {
    //     case HipUsersResponse(users) => 
    //       hipChatUsers = users
    //       collectUsers()
    //   }
    // }
    //
    // def rosterEntries() = {
    //   context.actorOf(Props(classOf[RosterActor], connection)) ! RosterList()
    //   expectOnce {
    //     case RosterResponse(roster) => 
    //       rosterEntries = roster
    //       collectUsers()
    //   }
    // }

    def collectUsers(force: Boolean = false) = {
      if (hipChatUsers != Nil) log.info("HIPCHAT USERS IS NOT NIL")
      else log.error("HIPCHAT USERS IS NIL")
      if (rosterEntries != Nil) log.info("Roster ENTRIES IS NOT NIL")
      else log.error("ROSTER ENTRIES IS NIL")
      if ((hipChatUsers != Nil && rosterEntries != Nil) || force) {
        // hipchat doesn't return the jid so let's convert it
        val hcMap = hipChatUsers map { u =>
          s"1_${u.id}@chat.btf.hipchat.com" -> u
        } toMap

        pprint.log(hcMap.size,"HC MAP")
        pprint.log(hipChatUsers.length,"users")
        val results: List[User] = rosterEntries map { re =>
          if (hcMap.get(re.getUser()) == None) {
            // log.warning(s"USER DOES NOT EXIST ${re}")
            User(
              name     = re.getName(),
              jid      = re.getUser(),
              nick     = "tbd",
              email    = "donotreply@rallyhealth.com",//p.email,
              timezone = Timezone("UTC", -420.0),
              entry    = re
            ) 
          } else {
            User(
              name     = re.getName(),
              jid      = re.getUser(),
              nick     = hcMap(re.getUser()).mention_name,
              email    = hcMap(re.getUser()).email.getOrElse("donotreply@rallyhealth.com"),//p.email,
              timezone = Timezone("UTC", -420.0),
              entry    = re
            ) 
          }
        }
        originalSender ! UserListResponse(results.toList) // Make sure it becomes immutable
        context.stop(self)
      }
    }
  }
}
