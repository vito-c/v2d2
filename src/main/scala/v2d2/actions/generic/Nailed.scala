package v2d2.actions.generic

import akka.actor.{Actor, ActorContext, Props, ActorLogging}
import v2d2.client.IMessage
import scala.util.Random
import v2d2.actions.generic.protocol.Nailed
import v2d2.client.core.Test

class NailedIt extends Actor with ActorLogging {

  val pics: List[String] = List(
    "http://i1.kym-cdn.com/photos/images/newsfeed/000/491/935/b7a.gif",
    "https://carlypea.files.wordpress.com/2013/02/snailed-it1.jpg",
    "http://runt-of-the-web.com/wordpress/wp-content/uploads/2014/04/batman.jpg"
  )

  def receive: Receive = {
    case Nailed(imsg) =>
      context.parent ! "" + Random.shuffle(pics).head
    case imsg: IMessage =>
      Nailed(imsg) match {
        case Some(cmd) =>
          context.parent ! "" + Random.shuffle(pics).head
        case _ => None
      }
    case _ => None
  }
}
