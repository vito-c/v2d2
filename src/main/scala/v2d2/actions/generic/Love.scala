package v2d2.actions.generic

import akka.actor.{ActorRef, Actor, ActorSystem, ActorContext, Props, ActorLogging}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
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
import v2d2.client.core._

class Lover(muc: MultiUserChat) extends Actor with ActorLogging with LoveJsonProtocol {

  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25.seconds)

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

    case love: SendUsersLove =>
      val request = for {
        sender <- (self ? love.sender).mapTo[LoveNickResult]
        takers <- Future.sequence(love.receivers map { u => (self ? u).mapTo[LoveNickResult] })
      } yield(
        SendLoves(
          love.sender,
          love.receivers,
          sender.nickname,
          takers map { n => n.nickname },
          love.reason.getOrElse("something random"),
          love.imsg))
      request onComplete {
        case Success(sendLoves) => self ! sendLoves
        case Failure(t) =>
          context.parent ! "An error has occured: " + t.getMessage
          context.parent ! "Rest api services that use php are sooo early 2000s"
      }

    case loves: SendLoves =>
      val datas: Seq[RequestEntity] = loves.receiverNicks map { n =>
        Await.result(
          Marshal(FormData(
            "action"  -> "sendlovemsg",
            "caller"  -> "hippybot",
            "api_key" -> V2D2.sendLove("api_key"),
            "from"    -> loves.senderNick,
            "to"      -> n,
            "why" -> loves.reason, "priv" -> "0")).to[RequestEntity], 1.second)
      }
      val request: Future[Seq[SendLoveResult]] = Future.sequence( 
      datas map { d => 
        Http().singleRequest(
          HttpRequest(
            uri = V2D2.sendLove("url"),
            method = HttpMethods.POST, entity = d)
        ).flatMap { response =>
          val fixed = response.mapEntity(
            _.withContentType(ContentTypes.`application/json`))
          Unmarshal(fixed).to[SendLoveResult]
        }
      })
      request onComplete {
        case Success(result) =>
          self ! LoveSuccess(loves.sender, loves.receivers, loves.reason, loves.imsg)
        case Failure(t) =>
          context.parent ! "An error has occured: " + t.getMessage
          context.parent ! "My favorite subreddit is http://reddit.com/r/lolphp"
      }

    case success: LoveSuccess =>
      val nicks = success.receivers map { u => s"@${u.nick}" } mkString(" ")
      context.parent ! s"Love was sent to ${nicks}\n  - ${success.reason}"

    case imsg: IMessage =>
      val occupant = muc.getOccupant(imsg.fromRaw)
      val fJid = XmppStringUtils.parseBareJid(occupant.getJid())
      Love(imsg) match {
        case Some(love) =>
          log.info("request love")
          val req = for {
            nmap <- (context.actorSelection("/user/xmpp") ? NickMap()).mapTo[Map[String,User]]
            jmap <- (context.actorSelection("/user/xmpp") ? UserMap()).mapTo[Map[String,User]]
          } yield(
            jmap get (fJid) match {
              case Some(user) =>
                val sender = user
                log.info("sending love!!")
                val users  = love.targets flatMap { target =>
                  nmap get (target.trim) match {
                    case Some(user) if user.jid == V2D2.v2d2Jid =>
                      //snappy comeback plus send love
                      if (love.targets.length == 1) context.parent ! s"@${sender.nick} umm this is awkward... I am incapable of expressing emotions"
                      Nil
                    case Some(user) if user.jid == sender.jid =>
                      if (love.targets.length == 1) {
                        context.parent ! s"@${sender.nick} great job you REALLY broke my programming"
                      }
                      Nil
                    case Some(user) => 
                      log.info(s"get target ${user}")
                      List(user)
                    case _ =>
                      context.parent ! s"Nice try silly human."
                      Nil
                  }
                }
                self ! SendUsersLove(sender, users, love.reason, imsg)
              case _ =>
                context.parent ! s"..sigh humans."
          })
        case _ => None
      }
  }
}
