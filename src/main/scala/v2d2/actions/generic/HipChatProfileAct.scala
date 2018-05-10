package v2d2.actions.generic.hipchat

import scala.concurrent.duration._
import org.jxmpp.jid.impl.JidCreate
import scala.util.{Failure, Success}
import org.jivesoftware.smack.roster.RosterEntry
import v2d2.V2D2
import concurrent.Future

import akka.actor.{Actor, ActorContext, ActorRef, ActorLogging, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.http.scaladsl.Http
import scala.collection.mutable.ArrayBuffer
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import akka.util.Timeout
import spray.json.DefaultJsonProtocol
import spray.json._
import akka.contrib.pattern.Aggregator

// API=https://hipchat.rallyhealth.com/v2
// AUTH_TOKEN=96nHrRVzKRXJsccjHuNS6K6X8WNmJrafF8TVpY70
// ROOM_ID=120
// MESSAGE="<pre>test notif</pre>"
// curl -H "Content-Type: application/json" \
//      -X POST \
//      -d "{\"color\": \"yellow\", \"message_format\": 
//      \"html\", \"message\": \"$MESSAGE\" }" \
//      $API/room/$ROOM_ID/notification?auth_token=$AUTH_TOKEN
//
case class TimedOut()
case class HipUsersReq( 
  start: Int = 0,
  max: Int = 500)

trait HipLinkProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val hipLinkFormat = jsonFormat3(HipLink.apply)
}
object HipLinkProtocol extends HipLinkProtocol

trait PresenceProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val hipLinkFormat = jsonFormat2(Presence.apply)
}
object PresenceProtocol extends PresenceProtocol

trait HipUserProtocol 
extends SprayJsonSupport 
with DefaultJsonProtocol {
  implicit val presenceFormatB = jsonFormat2(Presence.apply)
  implicit val hipLinkFormatB  = jsonFormat3(HipLink.apply)
  implicit val hipUserFormatB  = jsonFormat13(HipProfile.apply)
}
object HipUserProtocol extends HipUserProtocol

trait HipUsersProtocol
extends SprayJsonSupport
with DefaultJsonProtocol {
  implicit val presenceFormatA = jsonFormat2(Presence.apply)
  implicit val hipLinkFormatA  = jsonFormat3(HipLink.apply)
  implicit val hipUserFormatA  = jsonFormat13(HipProfile.apply)
  implicit val hipUsersFormatA = jsonFormat4(HipUsers.apply)
}
object HipUsersProtocol extends HipUsersProtocol

case class HipLink(
  next: Option[String],
  prev: Option[String],
  self: String
)
case class HipUsers(
  items: Seq[HipProfile],
  links: HipLink,
  maxResults: Int,
  startIndex: Int
)

case class Presence(
  is_online: Boolean,
  show: String
)

case class HipProfile(
  created: String,
  email: String,
  id: Int,
  is_deleted: Boolean,
  is_group_admin: Boolean,
  is_guest: Boolean,
  last_active: String,
  links: HipLink,
  mention_name: String,
  name: String,
  presence: Option[Presence],
  timezone: Option[String],
  xmpp_jid: Option[String]
) 

class HipChatProfileAct
extends Actor
with HipUsersProtocol
with ActorLogging {
  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25.seconds)

  def receive: Receive = {
    case entry: RosterEntry =>
      val jid = JidCreate.bareFrom(entry.getJid())
      pprint.log(jid, "bare jid")
      pprint.log(jid.toString(), "bare jid")
      val id  = try { jid.toString().split("@")(0).split("_")(1) } catch {
        case t:Throwable => "1" 
      }

      (for {
         response <- Http().singleRequest(
           HttpRequest(
             method = HttpMethods.GET,
             headers = List(headers.Accept(MediaTypes.`application/json`)),
             uri = s"https://hipchat.rallyhealth.com/v2/user/${id}?auth_token=${V2D2.hcapi}"))
         entity <- Unmarshal(response.entity).to[HipProfile]
       } yield { 
         log.info("REQUEST FINISHED")
         pprint.log(entity, "entity is right here")
       })
  }

}

// class HCUserHelper 
// extends Actor
// with HipUserProtocol
// with HipLinkProtocol
// with HipUsersProtocol
// with ActorLogging {
//
//   import system.dispatcher
//   implicit val system = ActorSystem()
//   implicit val materializer = ActorMaterializer()
//   implicit val timeout = Timeout(25.seconds)
//
//   def receive: Receive = {
//     case usrs: HipUsers =>
//       log.info(s"next: ${usrs.links.next.getOrElse("invalid")}&auth_token=${V2D2.hcapi}")
//       if (usrs.links.next == None) {
//         sender() ! HipUsersResponse(usrs.items.toList)
//       } else {
//         self ! GetHipUsers(Some(usrs), usrs.startIndex, usrs.maxResults)
//       }
//
//     case GetAllHipUsers() =>
//       self ! GetHipUsers()
//
//     case GetHipUsers(Some(users), start, max) =>
//       log.info(s"get: https://hipchat.rallyhealth.com/v2/user?start-index=${start}&max-results=${max}&auth_token=${V2D2.hcapi}")
//       sender() ! (for {
//           response <- Http().singleRequest(
//             HttpRequest(
//               method = HttpMethods.GET, 
//               headers = List(headers.Accept(MediaTypes.`application/json`)),
//               uri = s"https://hipchat.rallyhealth.com/v2/user?start-index=${start}&max-results=${max}&auth_token=${V2D2.hcapi}"))
//           entity <- Unmarshal(response.entity).to[HipUsers]
//         } yield { 
//           HipUsers(entity.items ++ users.items, entity.links, entity.maxResults, entity.startIndex)
//         })//) pipeTo sender()
//   }
// }

// import scala.concurrent.ExecutionContext.Implicits.global
// class HipChatUserAct 
// extends Actor 
// with HipUsersProtocol
// with ActorLogging {
//   import system.dispatcher
//   implicit val system = ActorSystem()
//   implicit val materializer: ActorMaterializer = ActorMaterializer()
//   implicit val timeout = Timeout(300.seconds)
//
//   def receive: Receive = {
//     case user: HipUser =>
//       (for {
//          response <- Http().singleRequest(
//            HttpRequest(
//              method = HttpMethods.GET, 
//              headers = List(headers.Accept(MediaTypes.`application/json`)),
//              uri = s"${user.links.self}?auth_token=${V2D2.hcapi}"))
//          limit <- response.headers.filter( _.lowercaseName() == "x-ratelimit-limit" )
//          entity <- Unmarshal(response.entity).to[HipUser]
//        } yield { 
//          log.info(s"RATE LIMIT IS: ${limit.value()}")
//          entity }) pipeTo sender()
//   }
// }

// class HipChatUserAct 
// extends Actor 
// with HipUsersProtocol
// with ActorLogging {
//   import system.dispatcher
//   implicit val system = ActorSystem()
//   implicit val materializer = ActorMaterializer()
//   def receive: Receive = {
//     case req: GetHipUsers =>
//       val start = req.start
//       val max = req.max
//       (for {
//          response <- Http().singleRequest(
//            HttpRequest(
//              method = HttpMethods.GET,
//              headers = List(headers.Accept(MediaTypes.`application/json`)),
//              uri = s"https://hipchat.rallyhealth.com/v2/user?start-index=${start}&max-results=${max}&auth_token=${V2D2.hcapi}"))
//          entity <- Unmarshal(response.entity).to[HipUsers]
//        } yield { 
//          log.info("REQUEST FINISHED")
//          req.users match {
//            case None =>
//              log.info("KICK OFF USERS GET")
//              HipUsers(entity.items, entity.links, entity.maxResults, entity.items.length)
//            case Some(users) =>
//              log.info(s"ADDITIONAL GETS ${users.items.length}")
//              HipUsers(entity.items ++ users.items, entity.links, entity.maxResults, users.items.length + entity.items.length)
//          }
//        }) pipeTo sender()
//   }
// }
//
// class HipChatUsersAct 
// extends Actor 
// with Aggregator
// with ActorLogging {
//
//   implicit val system = ActorSystem()
//   implicit val materializer = ActorMaterializer()
//   implicit val timeout = Timeout(300.seconds)
//
//   expectOnce {
//     case GetAllHipUsers() => new HipUserAggregator(sender())
//     case _ => context.stop(self)
//   }
//
//   class HipUserAggregator(originalSender: ActorRef) {
//     pprint.log("INSIDE HIP usr agg: " + originalSender)
//
//     import context.dispatcher
//
//     val userAcquirer = context.actorOf(Props(classOf[HipChatUserAct]))
//     context.system.scheduler.scheduleOnce(300.second, self, TimedOut)
//     userAcquirer ! GetHipUsers()
//     var users = List.empty[HipUser]
//     var usersFinal = ArrayBuffer.empty[HipUser]
// 	var index = 0
//
//     val handle = expect {
//       case peeps: HipUsers =>
//         if (peeps.links.next == None) {
//           users = peeps.items.toList
//           log.info(s"USERS ${users.length} PEEPS: ${peeps.items.length}")
//           pprint.log(users.head, "users done")
//
//           usersFinal = users.to[ArrayBuffer]
//           log.info(s"ADDING USER final: ${usersFinal.length} users: ${users.length}")
//           if (users.length == usersFinal.length) {
//             log.info("ALL DONE DONE DONE")
//             processUsers()
//           } else {
//             log.info("BAD NEWS BAD NEWS")
//           }
//         } else {
//           // users = peeps.items.toList
//           log.info(s"USERS ${users.length} PEEPS: ${peeps.items.length}")
//           log.info(s"GET MORE ${users.length} start: ${peeps.startIndex} max: ${peeps.maxResults}")
//           userAcquirer ! GetHipUsers(Some(peeps), peeps.startIndex, peeps.maxResults)
//         }
//
//       case TimedOut =>
//         log.info(s"TIME OUT IN HC")
//         pprint.log(users,"users")
//         pprint.log(usersFinal,"usersFinal")
//         processUsers()
//     }
//
//     def processUsers(): Unit = {
//       unexpect(handle)
//       pprint.log(usersFinal.head, "process")
//       originalSender ! HipUsersResponse(usersFinal.toList)
//       context.stop(self)
//     }
//     
//   }
// }
