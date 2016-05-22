package v2d2.actions.knock

import akka.actor.{ActorRef, Actor, ActorSystem, ActorContext, Props, ActorLogging}
import collection.JavaConversions._
import concurrent.Future
import concurrent.Promise
import java.util.Collection
import org.jivesoftware.smackx.muc.MultiUserChat
import org.jxmpp.util.XmppStringUtils
import org.jivesoftware.smackx.muc.Occupant
import scala.collection.immutable
import scala.concurrent.duration._
import scala.util.Random
import v2d2.V2D2
import v2d2.client.IMessage
import akka.pattern.{ask, pipe}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import v2d2.client.{IMessage,User}
import v2d2.actions.generic.protocol._
//TODO: change to protocol
import v2d2.client.core._

class Knocker(muc: MultiUserChat) extends Actor with ActorLogging {
  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25.seconds)

  case class Joke(target:String, state:Int, sender:String, jokeIdx:Int)

  val answers = Answers.answers
  val clues = Clues.clues
  var targets: Map[String, Joke] = Map()

  def receive: Receive = {

    case knock: KnockKnock =>
      context.parent ! s"knock knock case "
      for {
        nmap <- (context.actorSelection("/user/xmpp") ? NickMap()).mapTo[Map[String,User]]
      } yield {
        val nick = knock.target.getOrElse("fail")
        val jid  = knock.imsg.fromJid
        nmap get (nick.trim) match {
          case Some(user) => // if user.jid != V2D2.v2d2Jid =>
            context.parent ! s"starting knock knock joke with ${nick}"
            val jk = targets.getOrElse(
              user.jid,
              Joke(
                target = user.jid,
                state = 0,
                sender = jid,
                jokeIdx = Random.nextInt(clues.size)))
            if (jk.state < 1) {
              targets = targets.updated(user.jid, jk.copy(state = jk.state + 1))
              log.info(s"user ${user.jid} added to map")
              context.parent ! s"Knock, knock @${nick}!"
            } else {
              context.parent ! s"I already have a joke going with, @${nick}"
            }
          case _ => // nick does not exist - or trying ot prank bot
            context.parent ! s"Nice try silly human."
        }
      }

    case whois: Whois =>
      for {
        jmap <- (context.actorSelection("/user/xmpp") ? UserMap()).mapTo[Map[String,User]]
      } yield {
        val jid = whois.imsg.fromJid
        jmap get (jid) match {
          case Some(user) =>
            val jk = targets.getOrElse(
              user.jid,
              Joke(target = user.jid, state = 0, sender = jid, jokeIdx = 0))
            log.info(s"ujid: ${user.jid}")
            log.info(s"unick: ${user.nick}")
            if (jk.state == 1) {
              log.info(s"success ")
              targets = targets.updated(user.jid, jk.copy(state = jk.state + 1))
              context.parent ! s"@${user.nick}, ${clues(jk.jokeIdx)}"
            } else if (jk.state > 0) {
              log.info(s"snappy comeback")
              context.parent ! s"@${user.nick}, you do remember how knock knock jokes work?"
            } else {
              log.info(s"snap two")
              context.parent ! s"@${user.nick}, shhh don't try to steal jokes..."
            }
          case _ =>
            log.warning(s"user ${jid} not found")
            None
        }
      }

    case who: Who =>
      for {
        jmap <- (context.actorSelection("/user/xmpp") ? UserMap()).mapTo[Map[String,User]]
      } yield {
        val jid = who.imsg.fromJid
        jmap get (jid) match {
          case Some(user) =>
            val jk = targets.getOrElse(
              user.jid,
              Joke(
                target = user.jid,
                state = 0,
                sender = jid,
                jokeIdx = Random.nextInt(clues.size)))
            if (jk.state == 2) {
              context.parent ! s"@${user.nick}, ${answers(jk.jokeIdx)}"
              val jokeSender = jmap.getOrElse(jk.sender,
                throw new RuntimeException("This should never happen"))
              context.parent ! s"This joke was brough to you by: @${jokeSender.nick}"
              targets = targets - user.jid
            } else if (jk.state  > 0) {
              context.parent ! s"@${user.nick}, do you remember how knock knock jokes work?"
            } else {
              context.parent ! s"@${user.nick}, shhh don't try to steal jokes..."
            }
          case _ => None
        }
      }

    case imsg: IMessage =>
      // val occupant = muc.getOccupant(imsg.fromRaw)
      // val entry = V2D2.roster().getEntry(occupant.getJid())
      // val fromRaw = occupant.getJid()
      // val fromJid = XmppStringUtils.parseBareJid(fromRaw)
      // log.info(
      //   s"\n===========================================" +
      //   s"\n\tmsgfrmraw: ${imsg.fromRaw}" +
      //   s"\n\tmsgfrm: ${imsg.fromJid}" +
      //   s"\n\tfrmJid: ${fromJid}" +
      //   s"\n\toccupant.getNick: ${occupant.getNick()}" +
      //   s"\n\toccupant.getJid: ${occupant.getJid()}" +
      //   s"\n\toccupant.getAffiliation: ${occupant.getAffiliation()}" +
      //   s"\n\tentry.getName: ${entry.getName()}" +
      //   s"\n\tentry.getStatus: ${entry.getStatus()}" +
      //   s"\n\tentry.getType: ${entry.getType()}" +
      //   s"\n===========================================")
      // log.info(
      //   s"\n===========================================" +
      //   s"\ntargets: " +
      //   s"\n${targets}" +
      //   s"\n===========================================")

      if(KnockKnock(imsg) != None) self ! KnockKnock(imsg)
      else if(Whois(imsg) != None) self ! Whois(imsg)
      else if(Who(imsg) != None)   self ! Who(imsg)
      else { // if( targets.length > 0 ) {
        val jid = imsg.fromJid
        for {
          jmap <- (context.actorSelection("/user/xmpp") ? UserMap()).mapTo[Map[String,User]]
        } yield {
            jmap get (jid) match {
              case Some(user) =>
                context.parent ! s"user found: ${user} ${jid}"
                val jk = targets.getOrElse(
                  user.jid,
                  Joke(target = user.jid, state = 0, sender = jid, jokeIdx = 0))
                log.info(s"ujid: ${user.jid}")
                log.info(s"unick: ${user.nick}")
                if (jk.state >=1) {
                  //TODO: implement retrorts
                  context.parent ! s"@${user.nick} ಠ_ಠ"
                }
              case _ =>
                throw new RuntimeException(s"This should never happen ${jid} missing")
            }
        }
      }

      case _ => None
  }
}
