package v2d2.actions.generic

import scala.concurrent.duration._
import scala.util.{Failure, Success}
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
case class TimedOut()
case class HipUsersReq( 
  start: Int = 0,
  max: Int = 500)

trait HipLinkProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val hipLinkFormat = jsonFormat3(HipLink.apply)
}
object HipLinkProtocol extends HipLinkProtocol
case class HipLink(
  next: Option[String],
  prev: Option[String],
  self: String
)

trait HipUserProtocol 
extends SprayJsonSupport 
with DefaultJsonProtocol {
  implicit val hipLinkFormatB = jsonFormat3(HipLink.apply)
  implicit val hipUserFormatB = jsonFormat9(HipUser.apply)
}
object HipUserProtocol extends HipUserProtocol

trait HipUsersProtocol
extends SprayJsonSupport
with DefaultJsonProtocol {
  implicit val hipLinkFormatA = jsonFormat3(HipLink.apply)
  implicit val hipUserFormatA = jsonFormat9(HipUser.apply)
  implicit val hipUsersFormatA = jsonFormat4(HipUsers.apply)
}
object HipUsersProtocol extends HipUsersProtocol

case class HipUsers(
  items: Seq[HipUser],
  links: HipLink,
  maxResults: Int,
  startIndex: Int
)

case class HipUser(
  id: Int,
  mention_name: String,
  name: String,
  links: HipLink,
  email: Option[String],
  xmpp_jid: Option[String],
  is_deleted: Option[Boolean],
  is_guest: Option[Boolean],
  timezone: Option[String]
  // last_active
) 

case class HipUsersResponse(users: List[HipUser])
case class GetHipUsers(users:Option[HipUsers] = None, start:Int = 0, max:Int = 500)
case class GetAllHipUsers()
// {
//   "created": "2017-04-20T12:59:20+00:00",
//   "email": "vito.cutten@rallyhealth.com",
//   "group": {
//     "id": 1,
//     "links": {
//       "self": "https://hipchat.rallyhealth.com/v2/group/1"
//     },
//     "name": "RallyHealth"
//   },
//   "id": 492,
//   "is_deleted": false,
//   "is_group_admin": false,
//   "is_guest": false,
//   "last_active": "2018-02-08T05:09:55+0000",
//   "links": {
//     "self": "https://hipchat.rallyhealth.com/v2/user/492"
//   },
//   "mention_name": "ElonMusk",
//   "name": "Vito Cutten",
//   "photo_url": "https://hipchat.rallyhealth.com/files/photos/492/pjMzn9zeRCSTrsO_125.png",
//   "presence": {
//     "client": {
//       "type": "http://hipchat.com/client/mac/macweb",
//       "version": "749"
//     },
//     "is_online": true,
//     "show": "chat"
//   },
//   "roles": [
//     "user"
//   ],
//   "timezone": "America/Los_Angeles",
//   "title": "",
//   "version": "5L5MJLY2",
//   "xmpp_jid": "1_492@chat.btf.hipchat.com"
// }

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
class HipChatUserAct 
extends Actor 
with HipUsersProtocol
with ActorLogging {
  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val timeout = Timeout(300.seconds)

  def receive: Receive = {
    case p: HipUser =>
      (for {
         response <- Http().singleRequest(
           HttpRequest(
             method = HttpMethods.GET, 
             headers = List(headers.Accept(MediaTypes.`application/json`)),
             uri = s"${p.links.self}?auth_token=${V2D2.hcapi}"))
         entity <- Unmarshal(response.entity).to[HipUser]
       } yield { entity }) pipeTo sender()

    case req: GetHipUsers =>
      val start = req.start
      val max = req.max
      log.info(s"get: https://hipchat.rallyhealth.com/v2/user?start-index=${start}&max-results=${max}&auth_token=${V2D2.hcapi}")
      (for {
         response <- Http().singleRequest(
           HttpRequest(
             method = HttpMethods.GET, 
             headers = List(headers.Accept(MediaTypes.`application/json`)),
             uri = s"https://hipchat.rallyhealth.com/v2/user?start-index=${start}&max-results=${max}&auth_token=${V2D2.hcapi}"))
         entity <- Unmarshal(response.entity).to[HipUsers]
       } yield { 
         log.info("REQUEST FINISHED")
         req.users match {
           case None =>
             log.info("KICK OFF USERS GET")
             HipUsers(entity.items, entity.links, entity.maxResults, entity.items.length)
           case Some(users) =>
             log.info(s"ADDITIONAL GETS ${users.items.length}")
             HipUsers(entity.items ++ users.items, entity.links, entity.maxResults, users.items.length + entity.items.length)
         }
       }) pipeTo sender()
  }
}

class HipChatUsersAct 
extends Actor 
with Aggregator
with ActorLogging {
  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(300.seconds)

  expectOnce {
    // case GetHipUsers(Some(users), start, max) =>
    //   log.info(s"get: https://hipchat.rallyhealth.com/v2/user?start-index=${start}&max-results=${max}&auth_token=${V2D2.hcapi}")
    //   sender() ! (for {
    //       response <- Http().singleRequest(
    //         HttpRequest(
    //           method = HttpMethods.GET, 
    //           headers = List(headers.Accept(MediaTypes.`application/json`)),
    //           uri = s"https://hipchat.rallyhealth.com/v2/user?start-index=${start}&max-results=${max}&auth_token=${V2D2.hcapi}"))
    //       entity <- Unmarshal(response.entity).to[HipUsers]
    //     } yield { 
    //       pprint.log(entity)
    //       HipUsers(entity.items ++ users.items, entity.links, entity.maxResults, entity.startIndex)
    //     })
    case GetAllHipUsers() => new HipUserAggregator(sender())
    case _ => context.stop(self)
  }

  class HipUserAggregator(originalSender: ActorRef) {
    import context.dispatcher

    val userAcquirer = context.actorOf(Props(classOf[HipChatUserAct]))
    context.system.scheduler.scheduleOnce(300.second, self, TimedOut)
    userAcquirer ! GetHipUsers()
    var users = List.empty[HipUser]
    val usersFinal = ArrayBuffer.empty[HipUser]
	var index = 0

    val handle = expect {
      case peep: HipUser =>
        usersFinal += peep
		index += 1
        log.info(s"ADDING USER final: ${usersFinal.length} users: ${users.length}")
        if (users.length == usersFinal.length) {
          log.info("ALL DONE DONE DONE")
          processUsers()
        } else {
          userAcquirer ! users(index)
        }

      case peeps: HipUsers =>
        if (peeps.links.next == None) {
          users = peeps.items.toList
          log.info(s"USERS ${users.length} PEEPS: ${peeps.items.length}")
          userAcquirer ! users.head
        } else {
          // users = peeps.items.toList
          log.info(s"USERS ${users.length} PEEPS: ${peeps.items.length}")
          log.info(s"GET MORE ${users.length} start: ${peeps.startIndex} max: ${peeps.maxResults}")
          userAcquirer ! GetHipUsers(Some(peeps), peeps.startIndex, peeps.maxResults)
        }

      // case peep: HipUser =>
      //   log.info(s"ADDING USER final: ${usersFinal.length} users: ${users.length}")
      //   usersFinal += peep
      //   if ( finished && (usersFinal.length == users.length) ){
      //     log.info("ALL DONE DONE DONE")
      //     processUsers()
      //   }
      //
      // case peeps: HipUsers =>
      //   log.info("GET MORE USERS TO PROCESS")
      //   log.info(s"next: ${peeps.links.next.getOrElse("invalid")}&auth_token=${V2D2.hcapi}")
      //   if (peeps.links.next == None) {
      //     log.info("ALL DONE")
      //     finished = true
      //   } else {
      //     users ++= peeps.items.toList
      //     log.info(s"GET MORE ${users.length}")
      //     peeps.items map { p =>
      //       userAcquirer ! p
      //     }
      //     userAcquirer ! GetHipUsers(Some(peeps), peeps.startIndex, peeps.maxResults)
      //   }

      case TimedOut =>
        log.info(s"TIME OUT IN HC")
        pprint.log(users,"users")
        pprint.log(usersFinal,"usersFinal")
        processUsers()
    }

    def processUsers(): Unit = {
      unexpect(handle)
      originalSender ! HipUsersResponse(usersFinal.toList)
      context.stop(self)
    }
    
  }
}
//
//   import system.dispatcher
//   implicit val system = ActorSystem()
//   implicit val materializer = ActorMaterializer()
//   implicit val timeout = Timeout(25.seconds)
//
//   def getUsers(req:HipUsersReq): Future[HipUsers] = {
//     log.info(s"get: https://hipchat.rallyhealth.com/v2/user?start-index=${req.start}&max-results=${req.max}&auth_token=${V2D2.hcapi}")
//       val content = for {
//         response <- Http().singleRequest(
//           HttpRequest(
//             method = HttpMethods.GET, 
//             headers = List(headers.Accept(MediaTypes.`application/json`)),
//             uri = s"https://hipchat.rallyhealth.com/v2/user?start-index=${req.start}&max-results=${req.max}&auth_token=${V2D2.hcapi}"))
//         entity <- Unmarshal(response.entity).to[HipUsers]
//       } yield { 
//         entity
//       }
//       content onComplete {
//         case Success(data) =>
//           log.info(s"====================================================")
//           pprint.log(data.items.length,"users")
//           log.info(s"SUCCESS")
//           log.info(s"====================================================")
//         case Failure(t) =>
//           log.info(s"An error has occured: " + t.getMessage)
//       }
//       content
//   }
//
//   def getNext(res:HipUsers):Future[HipUsers] = {
//     log.info(s"next: ${res.links.next.getOrElse("invalid")}&auth_token=${V2D2.hcapi}")
//     for {
//       response <- Http().singleRequest(HttpRequest(
//           method = HttpMethods.GET, 
//           uri = s"${res.links.next.getOrElse("invalid")}&auth_token=${V2D2.hcapi}"))
//       entity <- {
//         log.info(s"RESPONSE: ${response}")
//         Unmarshal(response.entity).to[HipUsers]
//       }
//     } yield { 
//       entity
//     }
//   }
//
//   def getAllUsers(): Future[Seq[HipUser]] = {
//     def go(
//       future: Future[HipUsers],
//       users: Seq[HipUser]
//     ): Future[Seq[HipUser]] = {
//       future flatMap {
//         case u if u.links.next == None => 
//           Future.successful(u.items ++ users)
//         case u => 
//           go(getNext(u), users ++ u.items)
//       }
//     }
//     go(getUsers(HipUsersReq()), List[HipUser]())
//   }
//
//   def receive: Receive = {
//     case req: HipUsersReq => 
//       val content = for {
//         users <- getAllUsers()
//       } yield {
//         users
//       }
//       content onComplete {
//         case Success(data) =>
//           sender() ! HipUsersResponse(data.toList)
//         case Failure(t) =>
//           log.info(s"An error has occured: " + t.getMessage)
//       }
//
//     // case req: HipUsersReq =>
//     //   val content = for {
//     //     response <- Http().singleRequest(
//     //       HttpRequest(
//     //         method = HttpMethods.POST, 
//     //         uri = s"https://hipchat.rallyhealth.com/v2/user/?start-index=${req.start}&max-results=${req.max}&auth_token=${V2D2.hcapi}"))
//     //     entity <- Unmarshal(response.entity).to[HipUsers]
//     //   } yield { 
//     //     entity
//     //   }
//     //   content onComplete {
//     //     case Success(data) =>
//     //       log.info(s"Users ication has been sent ${data}")
//     //     case Failure(t) =>
//     //       log.info(s"An error has occured: " + t.getMessage)
//     //   }
//     case _ => None
//   }
// }
//
//
