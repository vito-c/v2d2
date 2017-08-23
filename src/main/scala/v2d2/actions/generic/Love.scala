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
import v2d2.actions.generic.protocol.LoveJsonProtocol._
import v2d2.actions.generic.protocol._
import v2d2.client.{IMessage,User}
import v2d2.client.core._

class Lover(muc: MultiUserChat) extends Actor with ActorLogging with LoveJsonProtocol {

  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25.seconds)

  def receive: Receive = {
    case imsg: IMessage =>
      Love(imsg) match {
        case Some(love) =>
          log.info(s"request love ${imsg.fromJid}")
          log.info(s"request love ${love}")
          for {
            nmap <- (context.actorSelection("/user/xmpp") ? NickMap()).mapTo[Map[String,User]]
            jmap <- (context.actorSelection("/user/xmpp") ? UserMap()).mapTo[Map[String,User]]
          } yield(
            jmap get (imsg.fromJid) match {
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
                      } else {
                        context.parent ! s"@${sender.nick} so clever hiding your nick in the list I almost didn't see it... I'll just send love to the others for you."
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
                log.info(s"${users}")
                if(users.length > 0)
                  self ! SendUsersLove(sender, users, love.reason, imsg)
              case _ =>
                context.parent ! s"..sigh humans."
          })
        case _ => None
      }

      case love: SendUsersLove =>
        val rlove = RallyLove(
          love.sender.email,
          love.recipients.map { usr => usr.email },
          love.message.getOrElse("something random"))
        val content = for {
          request <- Marshal(rlove).to[RequestEntity]
          response <- Http().singleRequest(
            HttpRequest(
              method = HttpMethods.POST, 
              uri = V2D2.sendLove("url"),
              entity = request))
          entity <- Unmarshal(response.entity).to[String]
        } yield entity
        content onComplete {
          case Success(sendLoves) => self ! sendLoves
            val nicks = love.recipients.map { usr => s"@${usr.nick}" }
            context.parent ! 
              s"Love has been sent to ${nicks.mkString(" ")}\n\t${love.message.getOrElse("something random")}"
          case Failure(t) =>
            context.parent ! "An error has occured: " + t.getMessage
        }
  }
}
