package v2d2.actions.generic

import scala.concurrent.duration._
import scala.util.{Failure, Success}
import v2d2.V2D2

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
trait HipNotifProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val HipNotifFormat = jsonFormat4(HipNotif.apply)
}
object HipNotifProtocol extends HipNotifProtocol
case class HipNotif( 
  color: String = "gray",
  message_format: String = "html",
  message: String,
  room: String)
class HipChatNotifs 
  extends Actor 
  with HipNotifProtocol
  with ActorLogging {

  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25.seconds)

  def receive: Receive = {
    case notif: HipNotif =>
      val content = for {
        request <- Marshal(notif).to[RequestEntity]
        response <- Http().singleRequest(
          HttpRequest(
            method = HttpMethods.POST, 
            uri = s"https://hipchat.rallyhealth.com/v2/room/${notif.room}/notification?auth_token=${V2D2.roomToken}",
            entity = request))
        entity <- Unmarshal(response.entity).to[String]
      } yield entity
      content onComplete {
        case Success(data) =>
          log.info(s"Notification has been sent ${data}")
        case Failure(t) =>
          log.info(s"An error has occured: " + t.getMessage)
      }
    case _ => None
  }
}

