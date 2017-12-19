package v2d2.client.core

import org.jivesoftware.smack.tcp.XMPPTCPConnection
import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem, Props}
import scala.concurrent.duration._
import akka.stream.ActorMaterializer
import akka.util.Timeout
import akka.pattern.{ask, pipe}
import v2d2.actions.generic._
import v2d2.client._
import scala.concurrent.{Future, Promise}
import org.jivesoftware.smack.StanzaListener
import scala.util.{Failure, Success}
import scala.collection.JavaConverters._
import org.jivesoftware.smack.packet.{Message, Presence, Stanza}

class ProfileActor(connection: XMPPTCPConnection) 
extends Actor 
with ActorLogging {

  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(100.seconds)

  def receive: Receive = {
    case rq: ProfileRQ =>
      val pp = Promise[Profile]()
      val f = Future {
        connection.sendIqWithResponseCallback(ProfileIQ(rq.jid), new StanzaListener() {
          def processStanza(packet: Stanza) = {
            if (packet != null && packet.isInstanceOf[ProfileIQ]) {
              val p:Profile = Profile(packet.asInstanceOf[ProfileIQ])
              if(!p.isInstanceOf[Profile]) {
                log.info(s"ERROR IN PROFILERQ ${p}")
              }
              pp.success(p)
            } else {
              pprint.log(rq, "Failure")
              pp.failure(//UserUseless(s"Failed: ${rq.jid}"))
                throw new Exception(s"failed ${rq.jid}"))
            }
          }
        })
      } 
      pp.future onComplete {
        case Success(r: Profile) =>
          if(!r.isInstanceOf[Profile]) {
            log.info(s"ERROR IN PROFILERQ ${r}")
          }
          //context.parent ! r
        case Failure(t) =>
          pprint.log(rq, "ERROR")
          // log.info(s"ERROR: ${t.getMessage} rq: ${rq}")
      }
      pp.future pipeTo sender
  }
}
