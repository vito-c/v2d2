package v2d2.client.core

import java.io.InputStream
import v2d2.client.User

import scala.collection.immutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}
// import v2d2.client.core.Email

import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.pattern.{ask, pipe}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import org.apache.commons.text.similarity._
import v2d2.actions.generic.HipNotif
import v2d2.actions.generic.protocol.Response
import v2d2.client.IMessage
import v2d2.parsers.FindUser
import scala.collection.immutable.SortedMap

class UserSearchAct
extends Actor
with ActorLogging {
  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25.seconds)

  // lazy val xmpp = context.actorSelection("/user/xmpp")
  // OutsideActor => UserSearchAct
  //                  | ==> Xmpp( get usermap)
  //                  | <== Xmpp respond
  //                  | do fuzzy searching
  //              <== | return result

  def searchMap(
    users: Map[String,User], 
    needle: String
  ): List[User] = {
    users.get(needle) match {
      case Some(u) => List(u)
      case None =>
        val jw = new JaroWinklerDistance()
        // val scores = (users.toList groupBy {
        //   i => jw.apply(i._1, needle)
        // }).toList.sortBy(_._1).last
        (users.toList.groupBy { 
          case(k,_) => jw.apply(k.toLowerCase,needle.toLowerCase)
        } maxBy { case(d,_) => d })._2 map { case(_,v) => v }
    }
  }

  def receive: Receive = {
    // case um:SearchUserMap =>
    //   for {
    //     asker <- um.searchable.asker
    //   } yield {
    //     asker ! um.map.get(um.searchable.needle)
    //   }

    case n: Nick =>
      log.info("nick request")
      (for {
        nmap <- (context.actorSelection("/user/xmpp") ? NickMap()).mapTo[Map[String,User]]
        usrs <- Future{searchMap(nmap, n.needle)}
      } yield { 
        usrs
      }) pipeTo sender

    // case j: JID =>
    //   log.info("jid request")
    //   (for {
    //     jmap <- (context.actorSelection("/user/xmpp") ? UserMap()).mapTo[UserMapResponse]
    //     usrs <- Future{searchMap(jmap.users, j.needle)}
    //   } yield { 
    //     usrs
    //   }) pipeTo sender

    case t @ (Email | UName) =>
      log.info("email request")
      val e = t.asInstanceOf[ISearchable]
      (for {
        emap <- (context.actorSelection("/user/xmpp") ? EmailMap()).mapTo[Map[String,User]]
        usrs <- Future{searchMap(emap, e.needle)}
      } yield { 
        usrs
      }) pipeTo sender

    case t @ (FullName(_)|Name(_)) =>
      log.info("name request")
      val n = t.asInstanceOf[ISearchable]
      (for {
        nmap <- (context.actorSelection("/user/xmpp") ? NameMap()).mapTo[Map[String,User]]
        usrs <- Future{searchMap(nmap, n.needle)}
      } yield {
        usrs
      }) pipeTo sender

    case f: FindUser =>
      log.info(s"find: ${f} and ${f.search}")
      for {
        sa <- f.search 
      } yield {
        self forward sa
      }
      //   context.actorSelection("/user/xmpp") ! Email(sa.needle, Some(sender))
      // }
    // case e: Email =>
    //   context.actorSelection("/user/xmpp") ! Email(e.needle, self)
          //
          // (for {
          //   emap <- (context.actorSelection("/user/xmpp") ? EmailMap()).mapTo[Map[String,User]]
          // } yield(emap.get(e.needle))) pipeTo sender
    // case Nick(nick) => ???
    // case FullName(name) => ???
    // case Name(name) => ???
  }
}
