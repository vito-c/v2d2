package v2d2.actions.generic

import akka.actor.{ActorRef, Actor, ActorSystem, ActorContext, Props, ActorLogging}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.pattern.{ask, pipe}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import collection.JavaConversions._
import concurrent.Future
import concurrent.Promise
import java.util.Collection
import org.jivesoftware.smackx.muc.MultiUserChat
import org.jxmpp.util.XmppStringUtils
import scala.collection.immutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import spray.client.pipelining._
import spray.httpx.SprayJsonSupport._
import spray.httpx.encoding.{Gzip, Deflate}
import spray.httpx.unmarshalling.FromResponseUnmarshaller
import v2d2.V2D2
import v2d2.actions.generic.protocol.LoveJsonProtocol
import v2d2.actions.generic.protocol.LoveJsonProtocol2._
import v2d2.actions.generic.protocol._
import v2d2.client.{IMessage,User}

class Lover(muc: MultiUserChat) extends Actor with ActorLogging with LoveJsonProtocol {

  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25 seconds)

  // Unmarshal(formdata).to[...] flatMap { data => Http.singleRequest(...)
  def receive: Receive = {
    case user: User =>
      val data: RequestEntity = Await.result(
        Marshal(FormData(
          "action"   -> V2D2.sendLove("nick"),
          "api_key"  -> V2D2.sendLove("api_key"),
          "username" -> user.email)).to[RequestEntity], 1.second)
      Http().singleRequest(
        HttpRequest(
          uri = V2D2.sendLove("url"),
          method = HttpMethods.POST, entity = data)
      ).flatMap { response =>
        val fixed = response.mapEntity(_.withContentType(ContentTypes.`application/json`))
        Unmarshal(fixed).to[LoveNickResult]
      } pipeTo sender
    case ulove: SendUserLove =>
      val users = for {
        sender   <- (self ? ulove.sender).mapTo[LoveNickResult]
        receiver <- (self ? ulove.receiver).mapTo[LoveNickResult]
      } yield(SendLove(sender.nickname, receiver.nickname, ulove.reason.getOrElse("something random")))
      users onComplete {
        case Success(love) => self ! love
        case Failure(t) =>
          context.parent ! "An error has occured: " + t.getMessage
          context.parent ! "Rest api services that use php are sooo early 2000s"
      }
    case love: SendLove =>
      val data: RequestEntity = Await.result(
        Marshal(FormData(
          "action"  -> "sendlovemsg",
          "caller"  -> "hippybot",
          "api_key" -> V2D2.sendLove("api_key"),
          "from"    -> love.sender,
          "to"      -> love.receiver,
          "why" -> love.reason, "priv" -> "0")).to[RequestEntity], 1.second)
      Http().singleRequest(
        HttpRequest(
          uri = V2D2.sendLove("url"),
          method = HttpMethods.POST, entity = data)
      ).flatMap { response =>
        val fixed = response.mapEntity(
          _.withContentType(ContentTypes.`application/json`))
        Unmarshal(fixed).to[SendLoveResult]
      } onComplete {
        case Success(result) =>
          context.parent ! s"Love has been sent to ${love.receiver}"
        case Failure(t) =>
          context.parent ! "An error has occured: " + t.getMessage
          context.parent ! "My favorite subreddit is http://reddit.com/r/lolphp"
      }
    case imsg: IMessage =>
      val occupant = muc.getOccupant(imsg.fromRaw)
      val fJid = XmppStringUtils.parseBareJid(occupant.getJid())
      Love(imsg) match {
        case Some(love) =>
          V2D2.nickMap onSuccess {
            case nmap =>
              V2D2.jidMap onSuccess {
                case jmap =>
                  jmap get (fJid) match {
                    case Some(user) =>
                      val sender = user
                      for (target <- love.targets) {
                        nmap get (target.trim) match {
                          // case Some(user) if user.jid == V2D2.v2d2Jid =>
                          //   //snappy comeback plus send love
                          //   context.parent ! s"@${sender.nick} umm this is awkward... I am incapable of expressing emotions"
                          case Some(user) =>
                            self ! SendUserLove(sender, user, love.reason)
                            // context.parent ! s"uj: ${user.email}"
                          case _ =>
                            context.parent ! s"Nice try silly human."
                        }
                      }
                    case _ =>
                      context.parent ! s"..sigh humans."
                  }
              }
          }
        case _ => None
      }
  }
}
