package v2d2.mtg

import akka.actor.ActorSystem
import v2d2.actions.generic.HipNotif
import v2d2.actions.generic.protocol.Response
import akka.testkit.{ ImplicitSender, TestActorRef, TestActors, TestKit }
import org.scalatest.{ BeforeAndAfterAll, Matchers, WordSpecLike }
import v2d2.client.{IMessage}
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

  case class TMsgData(content:String)
  class TMessage(data: TMsgData) extends IMessage {
    override def content: String = { data.content }
  }

  val testData = Map( 
    "a" -> TCardSet(
      name = "a",
      cards = List(
        TCard(name="sun", text=Some("sun")),
        TCard(name="fab lab", text=Some("fab lab")),
        TCard(name="gun", text=Some("gun")),
        TCard(name="xxxx sun avatar xxxx", text=Some("xxxx sun avatar xxxx")),
        TCard(name="yun", text=Some("yun"))
      )),
  "b" -> TCardSet(
    name = "b",
    cards = List(
      TCard(name="sun doo", text=Some("sun doo")),
      TCard(name="fab lab", text=Some("fab lab")),
      TCard(name="gun", text=Some("gun"))
    )),
  "c" -> TCardSet(
    name = "c",
    cards = List(
      TCard(name="sun boo", text=Some("sun boo")),
      TCard(name="fab lab", text=Some("fab lab")),
      TCard(name="gun", text=Some("gun"))
    ))
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
    
    "list scores correctly search w/o punctuation" in {
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
    }

    "list best match for card grouped on unique name" in {
      val actorRef = TestActorRef[MagicAct]
      val magic = actorRef.underlyingActor
      val s = magic.lookupName("gun", testData)
      assert(s.size == 1)
      assert(s.head.name == "gun")
    }

    "list best match for card grouped on unique name not exact" in {
      val actorRef = TestActorRef[MagicAct]
      val magic = actorRef.underlyingActor
      val s = magic.lookupName("gab lab", testData)
      assert(s.size == 1)
      assert(s.head.name == "fab lab")
    }

    "list best match for card with test data" in {
      val actorRef = TestActorRef[MagicAct]
      val magic = actorRef.underlyingActor
      val s = magic.lookupName("sun avatar", testData)
      assert(s.size == 1)
      assert(s.head.name == "xxxx sun avatar xxxx")
    }

    "list best match for card with real data" in {
      val proxy = TestProbe()
      val tmsg: IMessage = new TMessage(TMsgData("card name is sun's avatar?"))

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
      proxy.receiveN(1, 8.seconds)
    }

    "list best match for card with real data validate table lee swamp" in {
      val proxy = TestProbe()
      val tmsg: IMessage = new TMessage(TMsgData("card name is lee swamp?"))
      val cs = CardNameSearch(tmsg, "lee swamp")
      val parent = system.actorOf(Props(new Actor {
        val child = context.actorOf(Props(classOf[MagicAct]), "child")
        def receive = {
          case x if sender == child => 
            x match  {
              case r:Response =>
                println("==========================")
                println(r)
                println("==========================")
              case a:HipNotif =>
                println("==========================")
                println(a)
                val e = HipNotif("gray","html",
                  """<table><tr>
                    |<td><img src="https://magiccards.info/scans/en/m12/238.jpg" height="321"</td>
                    |<td><img src="https://magiccards.info/scans/en/ddr/65.jpg" height="321"</td>
                    |<td><img src="https://magiccards.info/scans/en/me2/243.jpg" height="321"</td>
                 |</tr></table>""".stripMargin.replaceAll("\n", ""))
                assert(e.toString == a.toString)
                println("==========================")
              case _ => assert(false)
            }
            proxy.ref forward x
          case x => child forward x
        }
      }))

      proxy.send(parent, tmsg)
      proxy.receiveN(1, 8.seconds)
    }

    "list best match for card with real data validate table sun" in {
      val proxy = TestProbe()
      val tmsg: IMessage = new TMessage(TMsgData("card name is sun?"))
      val cs = CardNameSearch(tmsg, "sun")
      val parent = system.actorOf(Props(new Actor {
        val child = context.actorOf(Props(classOf[MagicAct]), "child")
        def receive = {
          case x if sender == child => 
            x match  {
              case r:Response =>
                println("==========================")
                println(r)
                println("==========================")
              case a:HipNotif =>
                println("==========================")
                println(a)
                val e = HipNotif("gray","html",
                  """<table>
                      |<tr>
                        |<td><img src="https://magiccards.info/scans/en/ths/17.jpg" height="256"</td>
                        |<td><img src="https://magiccards.info/scans/en/c13/261.jpg" height="256"</td>
                        |<td><img src="https://magiccards.info/scans/en/xln/191b.jpg" height="256"</td>
                        |<td><img src="https://magiccards.info/scans/en/m12/39.jpg" height="256"</td>
                      |</tr>
                      |<tr>
                        |<td><img src="https://magiccards.info/scans/en/bng/13.jpg" height="256"</td>
                        |<td><img src="https://magiccards.info/scans/en/v11/12.jpg" height="256"</td>
                        |<td><img src="https://magiccards.info/scans/en/shm/243.jpg" height="256"</td>
                        |<td><img src="https://magiccards.info/scans/en/xln/27.jpg" height="256"</td>
                      |</tr>
                      |<tr>
                        |<td><img src="https://magiccards.info/scans/en/akh/4.jpg" height="256"</td>
                        |<td><img src="https://magiccards.info/scans/en/c14/233.jpg" height="256"</td>
                        |<td><img src="https://magiccards.info/scans/en/bng/22.jpg" height="256"</td>
                        |<td><img src="https://magiccards.info/scans/en/me3/52.jpg" height="256"</td>
                      |</tr>
                      |<tr>
                        |<td><img src="https://magiccards.info/scans/en/ptk/45.jpg" height="256"</td>
                        |<td><img src="https://magiccards.info/scans/en/gpt/109.jpg" height="256"</td>
                        |<td><img src="https://magiccards.info/scans/en/m14/222.jpg" height="256"</td>
                        |<td><img src="https://magiccards.info/scans/en/mrd/194.jpg" height="256"</td>
                     |</tr>
                  |</table>"""
                  .stripMargin.replaceAll("\n", ""))
                assert(e.toString == a.toString)
                println("==========================")
              case _ => assert(false)
            }
            proxy.ref forward x
          case x => child forward x
        }
      }))

      proxy.send(parent, tmsg)
      proxy.receiveN(1, 8.seconds)
    }
  }
}
