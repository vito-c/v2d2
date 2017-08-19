package v2d2.actions.love

import org.apache.commons.lang3.StringUtils
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
import v2d2.actions.generic.protocol.LoveListJPTL
import v2d2.actions.generic.protocol.LoveListJPTL._
import v2d2.actions.generic.protocol.LoveResultJPTL
import v2d2.actions.generic.protocol.LoveResultJPTL._
import v2d2.actions.generic.protocol.WhoJPTL._
import v2d2.actions.generic.protocol._
import v2d2.client.{IMessage,User}
import v2d2.client.core._

class WhoLoveAct extends Actor with ActorLogging with LoveResultJPTL {

  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25.seconds)
  def best(s: String, person: WhoUser): Int = {
    val f = StringUtils.getLevenshteinDistance(
      person.first.toLowerCase(), s)
    val g = StringUtils.getLevenshteinDistance(
      person.gitHubUsername.getOrElse("").toLowerCase(), s)
    val h = StringUtils.getLevenshteinDistance(
      person.hipchatMention.getOrElse("").toLowerCase(), s)
    if (f < g && f < h) f
    else if( g < f && g < h) g
    else h
  }

  def lookup(
    search: String, 
    people: Seq[WhoUser],
    nickMap: Map[String,User],
    emailMap: Map[String,User]
  ): Seq[WhoUser] = {
    log.info("in lookup")
    // first we look for literal matches on full name, first name, gitHubUsername
    // we already checked literal matches of user.name (email), first.last and hipchatmention
    // then we need to figure out the format of the string
    // "user.name" => check email filed
    // "first last" => check the name field
    // "someString" => we will just default for this ie _
    //    Then we will need to be createive here:
    //      check first name, githubusername, hipchatMention
    val uname = s"([^\\.]*\\.[^\\.]*)".r
    val fname = s"([^\\.]*\\s[^\\.]*)".r
    people.filter( p =>
      p.first.equalsIgnoreCase(search) ||
      p.name.equalsIgnoreCase(search) || 
      p.gitHubUsername.getOrElse("").equalsIgnoreCase(search)) match {
        case Nil =>
          log.info("in lookup NIL")
          search match {
            case uname(n) =>
              log.info("in lookup UNAME")
              // group by distance sort by distance take the closest list
              val out = people.groupBy( p =>
                StringUtils.getLevenshteinDistance(
                  p.email.toLowerCase(), n.toLowerCase())
              ).toList.sortBy(_._1).head._2
              log.info(s"in lookup $out")
              out
            case fname(n) =>
              log.info("in lookup fullname")
              people.groupBy( p =>
                StringUtils.getLevenshteinDistance(
                  p.name.toLowerCase(), n.toLowerCase())
              ).toList.sortBy(_._1).head._2
            case _ =>
              // check first name, githubusername, hipchatMention
              people.groupBy( p =>
                  best(search.toLowerCase(), p)
              ).toList.sortBy(_._1).head._2
          }
        case p => 
          log.info("found someone " + p)
          p
      }
  }

  def receive: Receive = {
    case who: GetWhoUser =>
      val target = who.target.email.replaceAll("@.*$","")
      val uri = s"${V2D2.who("url")}/people/${target}"
      log.info(s"request love for ${uri}")
      val content = for {
        response <- Http().singleRequest(
          HttpRequest(
            method = HttpMethods.GET,
            uri = s"${V2D2.who("url")}/people/${target}"))
        entity <- Unmarshal(response.entity).to[WhoUser]
      } yield entity
      content onComplete {
        case Success(data) =>
          log.info(s"output ${data}")
          context.parent ! Response(who.imsg,
            s"name: ${data.name}\n" +
            s"email: ${data.email}\n" +
            s"location: ${data.loc.getOrElse("No Data")}\n" +
            s"hipchat: ${data.hipchatMention.getOrElse("No Data")}\n" +
            s"avatar: ${V2D2.who("root")}${data.avatar.getOrElse("No Data")}"
            )
        case Failure(t) =>
          context.parent ! Response(who.imsg, s"An error has occured: " + t.getMessage)
      }

    case who: GetWhoAll =>
      val content = for {
        response <- Http().singleRequest(
          HttpRequest(
            method = HttpMethods.GET,
            uri = s"${V2D2.who("url")}/people"))
        entity <- Unmarshal(response.entity).to[Seq[WhoUser]]
      } yield entity
      content onComplete {
        case Success(data) =>
          // val res = data.filter(_.first.equalsIgnoreCase(who.search))
  // "name": "first": "email": (user.name) gitHubUsername hipchatMention
          log.info(s"request list search: " + who.search)
          for {
            emap <- (context.actorSelection("/user/xmpp") ? EmailMap()).mapTo[Map[String,User]]
            nmap <- (context.actorSelection("/user/xmpp") ? NickMap()).mapTo[Map[String,User]]
          } yield(
            context.parent ! Response(who.imsg,
              lookup(who.search, data, nmap, emap).map { d =>
                s"name: ${d.name}\n" +
                s"email: ${d.email}\n" +
                s"location: ${d.loc.getOrElse("No Data")}\n" +
                s"hipchat: ${d.hipchatMention.getOrElse("No Data")}\n" +
                s"avatar: ${V2D2.who("root")}${d.avatar.getOrElse("No Data")}"
            }.mkString("\n"))
          )

          // log.info("res: " + res + "target: " + who.search)
          // log.info(s"other output:\n" +
          //   res.map { d =>
          //     s"name: ${d.name}\n" +
          //     s"email: ${d.email}\n" +
          //     s"location: ${d.loc.getOrElse("No Data")}\n" +
          //     s"hipchat: ${d.hipchatMention.getOrElse("No Data")}\n" +
          //     s"avatar: ${V2D2.who("root")}${d.avatar.getOrElse("No Data")}"
          //   }.mkString("\n"))
          //
          // context.parent ! Response(who.imsg,
          //   res.map { d =>
          //     s"name: ${d.name}\n" +
          //     s"email: ${d.email}\n" +
          //     s"location: ${d.loc.getOrElse("No Data")}\n" +
          //     s"hipchat: ${d.hipchatMention.getOrElse("No Data")}\n" +
          //     s"avatar: ${V2D2.who("root")}${d.avatar.getOrElse("No Data")}"
          //   }.mkString("\n")
          // )
        case Failure(t) =>
          context.parent ! Response(who.imsg, s"An error has occured: " + t.getMessage)
      }

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
            context.parent ! Response(love.imsg, data.sent.map {
              res => s"${res.recipients.mkString(", ")} - ${res.message}"
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
            context.parent ! Response(love.imsg, data.received.map {
              res => s"${res.sender} - ${res.message}"
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

      WhoIs(imsg) match {
        case Some(who) =>
          log.info("entering who is")
          for {
            emap <- (context.actorSelection("/user/xmpp") ? EmailMap()).mapTo[Map[String,User]]
            nmap <- (context.actorSelection("/user/xmpp") ? NickMap()).mapTo[Map[String,User]]
          } yield(
            nmap get (who.target) match {
              case Some(user) =>
                log.info(s"who is ${who.target}")
                self ! GetWhoUser(imsg, user)
              case _ =>
                emap get (who.target + "@rallyhealth.com") match {
                  case Some(user) =>
                    log.info(s"who is ${who.target}")
                    self ! GetWhoUser(imsg, user)
                  case _ => self ! GetWhoAll(imsg, who.target)
                }
            })
        case _ => None
            // if(who.target.matches(s"[^\\.]*\\.[^\\.@]*")) {
            //   emap get (who.target + "@rallyhealth.com") match {
            //     case Some(user) =>
            //       log.info(s"who is ${who.target}")
            //       self ! GetWhoUser(imsg, user)
            //     case _ => None
            //   }
            // })
            //
            // jmap get (who.imsg.fromJid) match {
            //   case Some(user) =>
            //       log.info("who is user.foo")
            //       self ! GetWhoUser(who.target)
            //     } else {
            //       nmap get (who.target.trim) match {
            //         case Some(mark) => 
            //           self ! GetWhoUser(who.target)
            //         case _ =>
            //           self ! GetWhoAll(who.target)
            //       }
            //     }
            //     case _ =>
            //       context.parent ! Response(who.imsg, s"Nice try silly human.")
            // })
          // case _ => None
      }
      
  }
}
