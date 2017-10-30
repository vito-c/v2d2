package v2d2.mtg

import akka.actor.ActorSystem
import v2d2.actions.generic.HipNotif
import v2d2.actions.generic.protocol.Response
import akka.testkit.{ ImplicitSender, TestActorRef, TestActors, TestKit }
import org.scalatest.{ BeforeAndAfterAll, Matchers, WordSpecLike }
import v2d2.client.{IMessage, User}
import v2d2.client.core.MagicCards
import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import akka.testkit.TestProbe

case class TCardSet(name:String, cards:List[TCard]) extends ICardSet
case class TCard(
  name:String,
  setKey:Option[String] = None,
  manaCost: Option[String] = None,
  number:Option[String] = None,
  text: Option[String] = None
) extends ICard
class MagicSpec()
extends TestKit(ActorSystem("MagicSpec"))
with ImplicitSender
with WordSpecLike
with Matchers
with BeforeAndAfterAll {
  import system.dispatcher
  implicit val timeout = Timeout(25.seconds)

  class TMessage extends IMessage {
    override def content: String = { "card name is sun's avatar?" }
  }

  val testData = Map( 
    "a" -> TCardSet(
      name = "a",
      cards = List(
        TCard(name="sun"),
        TCard(name="fun"),
        TCard(name="gun"),
        TCard(name="poo poo sun avatar xxxxxxxxx"),
        TCard(name="yun")
      )),
  "b" -> TCardSet(
    name = "b",
    cards = List(TCard(name="sun doo"))),
  "c" -> TCardSet(
    name = "c",
    cards = List(TCard(name="sun poo")))
  )

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "A MagicAct" must {
    "return cards" in {
      val magic = system.actorOf(Props(classOf[MagicAct]))
      val content = magic ? MagicCards()
      content onComplete {
        case Success(sets) =>
          sets match {
            case s:Map[String,CardSet] =>
              s.size should be > 1
            case _ => assert(false)
          }
        case Failure(t) =>
          assert(false)
      }
      Await.result(content, 6.seconds)
      assert(content.isCompleted)
    }
    
    "list scores correctly punct" in {
      val actorRef = TestActorRef[MagicAct]
      val magic = actorRef.underlyingActor
      val s1 = magic.scores("the fox's fur", "fox's")
      val s2 = magic.scores("the fox's fur", "foxs")
      assert(s1 == s2)
    }

    "list scores correctly" in {
      val actorRef = TestActorRef[MagicAct]
      val magic = actorRef.underlyingActor
      val s = magic.scores("the quick brown fox", "fox")
      assert(s.size > 1)
      assert(s.head == 0)
    }

    "list scores correctly multi input" in {
      val actorRef = TestActorRef[MagicAct]
      val magic = actorRef.underlyingActor
      val s = magic.scores("sun", "sun avatar")
      assert(s.size == 1)
      assert(s.head == 0)
    }

    "list best match for card" in {
      val actorRef = TestActorRef[MagicAct]
      val magic = actorRef.underlyingActor
      val s = magic.lookupName("yun", testData)
      assert(s.size == 1)
      assert(s.head.name == "yun")
      // assert(s.head == 0)
    }

    "list best match for card with test data" in {
      val actorRef = TestActorRef[MagicAct]
      val magic = actorRef.underlyingActor
      val s = magic.lookupName("sun avatar", testData)
      assert(s.size == 1)
      assert(s.head.name == "poo poo sun avatar xxxxxxxxx")
    }

    "list best match for card with real data" in {
      val proxy = TestProbe()
      val tmsg: IMessage = new TMessage()
      val cs = CardNameSearch(tmsg, "sun's avatar")
      val parent = system.actorOf(Props(new Actor {
        val child = context.actorOf(Props(classOf[MagicAct]), "child")
        def receive = {
          case x if sender == child => 
            x match  {
              case r:Response =>
                println("==========================")
                println(r)
                println("==========================")
              case n:HipNotif =>
                println("==========================")
                println(n)
                println("==========================")
              case _ => assert(false)
            }
            proxy.ref forward x
          case x => child forward x
        }
      }))

      proxy.send(parent, tmsg)
      proxy.receiveN(4, 8.seconds)
    }
  }
}

