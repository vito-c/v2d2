package v2d2.client.core

import scala.collection.immutable
import scala.concurrent.duration._

import akka.actor.{Actor, ActorContext, ActorLogging, ActorRef, ActorSystem, Props}
import akka.contrib.pattern.Aggregator
import org.jivesoftware.smack.roster.RosterEntry
import org.jivesoftware.smack.tcp.XMPPTCPConnection
import v2d2.actions.generic._
import v2d2.actions.generic.hipchat._
import v2d2.client._
import v2d2.client.core._

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
    val profiler = context.actorOf(Props(classOf[HipChatProfileAct]))

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
        pprint.log(roster.head, "roster")
        profiler ! roster.head

        rosterEntries = roster
        collectUsers()
    } 

    context.system.scheduler.scheduleOnce(300.second, self, TimedOut)
    expectOnce {
      case TimedOut =>
        log.info(s"TIMED OUT")
        collectUsers(force = true)
    }


    def collectUsers(force: Boolean = false) = {

      if ((hipChatUsers != Nil && rosterEntries != Nil) || force) {
        // hipchat doesn't return the jid so let's convert it
        val hcMap = hipChatUsers map { u =>
          s"1_${u.id}@chat.btf.hipchat.com" -> u
        } toMap

        pprint.log(hcMap.size,"HC MAP")
        pprint.log(hipChatUsers.length,"users")
        val results: List[User] = rosterEntries map { re =>
          if (hcMap.get(re.getUser()) == None) {
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
              email    = hcMap(re.getUser()).email.getOrElse("donotreply@rallyhealth.com"),
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
