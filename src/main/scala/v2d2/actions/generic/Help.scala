package v2d2.actions.generic

import scala.util.Random

import akka.actor.{Actor, ActorContext, ActorLogging}
import v2d2.actions.generic.protocol.Helpme
import v2d2.client.IMessage

class Help extends Actor with ActorLogging {

  val pics: List[String] = List(
    "PRS soon I am accepting",
    "help help help",
    "help I need somebody... not just anybody",
    "(zlzf)"
  )

  def receive: Receive = {
    case h: Helpme =>
      context.parent ! "" + Random.shuffle(pics).head
    case imsg:IMessage =>
      Helpme(imsg) match {
        case Some(cmd) =>
          context.parent ! "" + Random.shuffle(pics).head
        case _ => None
      }
    case _ => None
  }
}
