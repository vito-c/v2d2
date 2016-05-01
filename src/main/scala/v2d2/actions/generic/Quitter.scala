package v2d2.actions.generic

import akka.actor.{Actor, ActorContext, Props, ActorLogging}
import v2d2.client.IMessage
import v2d2.actions.generic.protocol.Quit

class Quitter extends Actor with ActorLogging {

  // val actionBehavior: Receive = {
  //   case (imsg) =>
  //     context.parent ! s"good bye cruel world"
  //     context.system.terminate()
  // }

  def receive: Receive = {
    case Quit(imsg) =>
      context.parent ! s"the robocolpyse will come for you all!"
      context.system.terminate()
    // case QuitMsg(imsg) =>
    //   context.parent ! s"bollocks"
    //   context.system.terminate()
    case imsg:IMessage =>
        Quit(imsg) match {
          case Some(imsg) =>
            context.parent ! s"good bye cruel world"
            context.system.terminate()
          case _ => None
        }
    case _ => None
  }
}

