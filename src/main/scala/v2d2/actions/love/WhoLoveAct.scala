package v2d2.actions.love

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import v2d2.V2D2
import v2d2.actions.generic.protocol.Response
import v2d2.client.{IMessage, User}
import v2d2.client.core._

class WhoLoveAct 
  extends Actor 
  with ActorLogging
  with LoveListJPTL 
  with LoveResultJPTL {

  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25.seconds)

  def receive: Receive = {

    case love: GetUsersSentLove =>
      val target = love.target.email.replaceAll("@.*$","")
      val uri = s"${V2D2.sendLove("url")}/${target}"
      log.info(s"request love for ${uri}")
      val content = for {
        response <- Http().singleRequest(
          HttpRequest(
            method = HttpMethods.GET, 
            uri = s"${V2D2.sendLove("url")}/${target}"))
        entity <- Unmarshal(response.entity).to[LoveList]
      } yield entity
      content onComplete {
        case Success(data) =>
          log.info(s"output ${data.sent}")
          if(data.sent.length == 0) {
            context.parent ! Response(love.imsg, s"@${love.target.nick} has a heart of stone")
          } else {
            context.parent ! Response(love.imsg, s"${love.target.nick} has sent love to:\n" +
            data.sent.map {
              res => s"\t${res.recipients.mkString(", ")} - ${res.message}"
            }.mkString("\n"))
          }
        case Failure(t) =>
          context.parent ! Response(love.imsg, s"An error has occured: " + t.getMessage)
      }

    case love: GetUsersLove =>
      val target = love.target.email.replaceAll("@.*$","")
      val buri = s"${V2D2.sendLove("url")}/${target}"
      log.info(s"request love for ${buri}")
      val content = for {
        response <- Http().singleRequest(
          HttpRequest(
            method = HttpMethods.GET, 
            uri = s"${V2D2.sendLove("url")}/${target}"))
        entity <- Unmarshal(response.entity).to[LoveList]
      } yield entity
      content onComplete {
        case Success(data) =>
          log.info(s"output ${data.received}")
          if(data.received.length == 0) {
            context.parent ! Response(love.imsg, s"@${love.senderNick} oh boy this is awkward... you don't have any love yet")
          } else {
            context.parent ! Response(love.imsg, s"Loves sent to ${love.target.nick}:\n" +
              data.received.map {
                res => s"\t${res.sender} - ${res.message}"
            }.mkString("\n"))
          }
        case Failure(t) =>
          context.parent ! Response(love.imsg, s"An error has occured: " + t.getMessage)
      }

    case imsg: IMessage =>
      WhoDoLove(imsg) match {
        case Some(whodo) =>
          log.info(s"entering whodo")
          for {
            jmap <- (context.actorSelection("/user/xmpp") ? UserMap()).mapTo[Map[String,User]]
            nmap <- (context.actorSelection("/user/xmpp") ? NickMap()).mapTo[Map[String,User]]
          } yield(
            jmap get (whodo.imsg.fromJid) match {
              case Some(user) =>
                log.info(s"user: $user")
                if(whodo.target.equalsIgnoreCase("i")) {
                  self ! GetUsersSentLove(user, user.nick, whodo.imsg)
                } else {
                  log.info(s"WELL THIS whodo ${whodo.target}")
                  nmap get (whodo.target.trim) match {
                    case Some(mark) => 
                      self ! GetUsersSentLove(mark, user.nick, whodo.imsg)
                    case _ =>
                      context.parent ! Response(whodo.imsg, s"Nice try silly human.")
                  }
                }
                case _ =>
                  context.parent ! Response(whodo.imsg, s"Nice try silly human.")
            })
          case _ => None
      }

      WhoLove(imsg) match {
        case Some(who) =>
          log.info("entering who love")
          for {
            jmap <- (context.actorSelection("/user/xmpp") ? UserMap()).mapTo[Map[String,User]]
            nmap <- (context.actorSelection("/user/xmpp") ? NickMap()).mapTo[Map[String,User]]
          } yield(
            jmap get (who.imsg.fromJid) match {
              case Some(user) =>
                log.info(s"WELL THIS who ${who.target}")
                if(who.target.equalsIgnoreCase("me")) {
                  log.info("who talk to yourself")
                  self ! GetUsersLove(user, user.nick, who.imsg)
                } else {
                  nmap get (who.target.trim) match {
                    case Some(mark) => 
                      self ! GetUsersLove(mark, user.nick, who.imsg)
                    case _ =>
                      context.parent ! Response(who.imsg, s"Nice try silly human.")
                  }
                }
                case _ =>
                  context.parent ! Response(who.imsg, s"Nice try silly human.")
            })
          case _ => None
      }
  }
}
