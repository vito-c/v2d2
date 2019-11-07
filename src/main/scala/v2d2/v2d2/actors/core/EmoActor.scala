package v2d2.actors.core

import slack.rtm.SlackRtmClient
import akka.actor.{Actor, ActorContext, ActorLogging, Props}
import slack.SlackUtil
import v2d2.protocols.{Relay, SimpleResponse, TrackJoke}
import com.softwaremill.macwire._
import akka.actor.{ActorRef, ActorSystem, Props}
import com.softwaremill.macwire.akkasupport._
import com.softwaremill.tagging._
import v2d2.actors.comedy.Knocker
import v2d2.mtg.MagicAct
import slack.models.ReactionAdded
import v2d2.protocols.KnockJoke
import slack.models.Message
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import v2d2.protocols.EmoRelay
import slack.models.ReactionItemMessage
import v2d2.V2D2
import slack.api.BlockingSlackApiClient
import v2d2.protocols.SetupTracker
import scala.collection.mutable
import v2d2.protocols.TrackedJoke
import v2d2.protocols.Votes
import slack.models.Reply
import scala.concurrent.ExecutionContext.Implicits.global
import v2d2.protocols.EphemResponse

class EmoActor(
  client: SlackRtmClient
) extends Actor
  with ActorLogging {

  implicit val system                 = ActorSystem("slack")
  val hclient: BlockingSlackApiClient = BlockingSlackApiClient(V2D2.ptoken)
  val selfId                          = client.state.self.id

  private var jokes: Map[String, TrackedJoke] = Map.empty
  private var tmap: Map[Long, TrackJoke]      = Map.empty

  def unstring(
    ts: String
  ): Long = {
    val arr   = ts.split('.')
    val sec   = arr(0).toLong
    val nanos = arr(1).toInt
    sec * 1000 * 1000 + nanos
  }

  def restring(
    ts: Long
  ): String = {
    val secs  = ts / (1000 * 1000)
    val nanos = ts % (1000 * 1000)
    secs.toString + "." + f"${nanos}%06d"
  }

  def updateJoke(
    reaction: String,
    vote: Votes
  ) = {
    vote.copy(
      up = if (Votes.vote(reaction) > 0) {
        1
      } else { 0 } + vote.up,
      down = if (Votes.vote(reaction) < 0) {
        1
      } else { 0 } + vote.down,
      unknown = if (Votes.vote(reaction) == 0) {
        1
      } else { 0 } + vote.unknown,
      emojis = vote.emojis + (reaction -> (vote.emojis.getOrElse(reaction, 0) + 1))
    )
  }

  def receive: Receive = {
    case item: TrackJoke =>
      client.sendMessage(item.received.channel, item.deliver).map { id =>
        tmap += (id -> item)
      }

    case r: Reply =>
      tmap.get(r.reply_to).map { item =>
        jokes += (r.ts + item.channel -> TrackedJoke(
          joke = item.joke,
          votes = Votes(
            up = 0,
            down = 0,
            unknown = 0,
            emojis = scala.collection.immutable.Map.empty
          )
        ))
        // client.sendMessage(item.channel, s"Vote for this joke by responding with an emoji")
        tmap.remove(r.reply_to)
      }

    case r: ReactionAdded =>
      r.item match {
        case item: ReactionItemMessage =>
          val id = item.ts + item.channel
          if (jokes.contains(id)) {
            jokes = jokes.updated(
              id,
              jokes(id).copy(votes = updateJoke(r.reaction, jokes(id).votes))
            )

            val str = s"A vote ${if (Votes.vote(r.reaction) > 0) {
              "in favor of remembering this joke"
            } else if (Votes.vote(r.reaction) < 0) {
              "in favor of forgetting this joke"
            } else {
              s"for :${r.reaction}: "
            }} was cast!${if (Votes.vote(r.reaction) == 0) {
              s"...wait I don't know what to do with ${r.reaction}"
            } else ""}"
            context.actorSelection("/user/mux") ! EphemResponse(
              Message(
                ts = "",
                channel = item.channel,
                user = r.user,
                text = str,
                is_starred = None,
                thread_ts = None
              ),
              str
            )
            // client.sendMessage(item.channel,
            // s"A vote ${
            //     if(Votes.vote(r.reaction) > 0) { "in favor of remembering this joke" }
            //     else if(Votes.vote(r.reaction) < 0) { "in favor of forgetting this joke" }
            //     else { s"for :${r.reaction}: "}
            //   } was cast!${if(Votes.vote(r.reaction) == 0) { s"...wait I don't know what to do with ${r.reaction}" } }"
            // )
          }
        case _ =>
          pprint.log("hey")
      }
    case x =>
      pprint.log(x)
  }
}
