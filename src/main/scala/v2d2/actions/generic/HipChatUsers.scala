package v2d2.actions.generic

import scala.concurrent.duration._
import scala.util.{Failure, Success}
import v2d2.V2D2
import concurrent.Future

import akka.actor.{Actor, ActorLogging, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import akka.util.Timeout
import spray.json.DefaultJsonProtocol

// API=https://hipchat.rallyhealth.com/v2
// AUTH_TOKEN=96nHrRVzKRXJsccjHuNS6K6X8WNmJrafF8TVpY70
// ROOM_ID=120
// MESSAGE="<pre>test notif</pre>"
// curl -H "Content-Type: application/json" \
//      -X POST \
//      -d "{\"color\": \"yellow\", \"message_format\": 
//      \"html\", \"message\": \"$MESSAGE\" }" \
//      $API/room/$ROOM_ID/notification?auth_token=$AUTH_TOKEN
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

trait HipUsersProtocol
  extends SprayJsonSupport
  with DefaultJsonProtocol {
  implicit val hipUserFormatA = jsonFormat3(HipUser.apply)
  implicit val hipLinkFormatA = jsonFormat3(HipLink.apply)
  implicit val hipUsersFormat = jsonFormat4(HipUsers.apply)
}
object HipUsersProtocol extends HipUsersProtocol
case class HipUsers(
  items: Seq[HipUser],
  links: HipLink,
  maxResults: Int,
  startIndex: Int
)

trait HipUserProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val hipUserFormat = jsonFormat3(HipUser.apply)
}
object HipUserProtocol extends HipUserProtocol
case class HipUser(
  id: Int,
  mention_name: String,
  name: String
) 


class HipChatUsersAct 
  extends Actor 
  with HipUserProtocol
  with HipLinkProtocol
  with HipUsersProtocol
  with ActorLogging {

  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25.seconds)

  def getUsers(req:HipUsersReq): Future[HipUsers] = {
    log.info(s"get: https://hipchat.rallyhealth.com/v2/user?start-index=${req.start}&max-results=${req.max}&auth_token=${V2D2.hcapi}")
      val content = for {
        response <- Http().singleRequest(
          HttpRequest(
            method = HttpMethods.GET, 
            headers = List(headers.Accept(MediaTypes.`application/json`)),
            uri = s"https://hipchat.rallyhealth.com/v2/user?start-index=${req.start}&max-results=${req.max}&auth_token=${V2D2.hcapi}"))
        entity <- Unmarshal(response.entity).to[HipUsers]
      } yield { 
        entity
      }
      content onComplete {
        case Success(data) =>
          log.info(s"====================================================")
          pprint.log(data.items.length,"users")
          log.info(s"SUCCESS")
          log.info(s"====================================================")
        case Failure(t) =>
          log.info(s"An error has occured: " + t.getMessage)
      }
      content
  }

  def getNext(res:HipUsers):Future[HipUsers] = {
    log.info(s"next: ${res.links.next.getOrElse("invalid")}&auth_token=${V2D2.hcapi}")
    for {
      response <- Http().singleRequest(HttpRequest(
          method = HttpMethods.GET, 
          uri = s"${res.links.next.getOrElse("invalid")}&auth_token=${V2D2.hcapi}"))
      entity <- {
        log.info(s"RESPONSE: ${response}")
        Unmarshal(response.entity).to[HipUsers]
      }
    } yield { 
      entity
    }
  }

  def getAllUsers(): Future[Seq[HipUser]] = {
    def go(
      future:Future[HipUsers],
      users:Seq[HipUser]
    ): Future[Seq[HipUser]] = {
      future flatMap {
        case u if u.links.next == None => 
          Future.successful(u.items ++ users)
        case u => 
          go(getNext(u), users ++ u.items)
      }
    }
    go(getUsers(HipUsersReq()), List[HipUser]())
  }

  def receive: Receive = {
    case req: HipUsersReq => 
      val content = for {
        users <- getAllUsers()
      } yield {
        users
      }
      content onComplete {
        case Success(data) =>
          log.info(s"====================================================")
          pprint.log(data.length,"users")
          val me = data.filter( _.name == "Vito Cutten")
          pprint.log(me,"users")
          
          log.info(s"SUCCESS")
          log.info(s"====================================================")
        case Failure(t) =>
          log.info(s"An error has occured: " + t.getMessage)
      }
    // case req: HipUsersReq =>
    //   val content = for {
    //     response <- Http().singleRequest(
    //       HttpRequest(
    //         method = HttpMethods.POST, 
    //         uri = s"https://hipchat.rallyhealth.com/v2/user/?start-index=${req.start}&max-results=${req.max}&auth_token=${V2D2.hcapi}"))
    //     entity <- Unmarshal(response.entity).to[HipUsers]
    //   } yield { 
    //     entity
    //   }
    //   content onComplete {
    //     case Success(data) =>
    //       log.info(s"Users ication has been sent ${data}")
    //     case Failure(t) =>
    //       log.info(s"An error has occured: " + t.getMessage)
    //   }
    case _ => None
  }
}


