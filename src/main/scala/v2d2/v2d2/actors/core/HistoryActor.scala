package v2d2.actors.core

import slack.rtm.SlackRtmClient
import akka.actor.{Actor, ActorContext, ActorLogging, Props}
import slack.SlackUtil
import v2d2.protocols._
import com.softwaremill.macwire._
import akka.actor.{ActorRef, ActorSystem, Props}
import com.softwaremill.macwire.akkasupport._
import scala.concurrent.Future
import com.softwaremill.tagging._
import v2d2.actors.comedy.Knocker
import v2d2.mtg.MagicAct
import v2d2.V2D2

import slack.api.SlackApiClient
import slack.api.BlockingSlackApiClient

class HistoryActor(
  rtmclient: SlackRtmClient
) extends Actor
  with ActorLogging {

  implicit val system                = ActorSystem("slack")
  val client: BlockingSlackApiClient = BlockingSlackApiClient(V2D2.ptoken)

  def receive: Receive = {
    case History(channel, latest, oldest, inclusive, count) =>
      val chunk = client.getChannelHistory(
        channelId = channel,
        latest = latest,
        oldest = oldest,
        inclusive = inclusive,
        count = count
      )
      sender ! chunk

  }
}
