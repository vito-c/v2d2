package v2d2.actions.generic

import v2d2.actions.generic.protocol.Response
import scala.util.{Success, Failure}
import v2d2.client.IMessage
import akka.actor.{ActorRef, Actor, ActorSystem, ActorContext, Props, ActorLogging}
import akka.actor.{Actor, ActorContext, Props, ActorLogging}
import akka.stream.ActorMaterializer
import scala.concurrent.duration._
import akka.util.Timeout
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import scala.util.Random
import v2d2.actions.generic.protocol.Helpme
import spray.client.pipelining._
import spray.httpx.SprayJsonSupport._
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport

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
  implicit val sendLoveFormat = jsonFormat3(HipNotif.apply)
}
object HipNotifProtocol extends HipNotifProtocol
case class HipNotif( 
  color: String = "gray",
  message_format: String = "html",
  message: String)
class HipChatNotifs extends Actor with ActorLogging with HipNotifProtocol{

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
            uri = "https://hipchat.rallyhealth.com/v2/room/120/notification?auth_token=96nHrRVzKRXJsccjHuNS6K6X8WNmJrafF8TVpY70",
            entity = request))
        entity <- Unmarshal(response.entity).to[String]
      } yield entity
      content onComplete {
        case Success(data) =>
          log.info("Notification has been sent")
        case Failure(t) =>
          log.info(s"An error has occured: " + t.getMessage)
      }
    case _ => None
  }
}

