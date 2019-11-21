package v2d2.mtg

import org.apache.commons.text.similarity._
import akka.actor.ActorSystem
import v2d2.protocols.Response
import akka.testkit.{ImplicitSender, TestActorRef, TestActors, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import slack.models.Message
import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import akka.testkit.TestProbe

case class TCardSet(
  name: String,
  cards: List[TCard]
) extends ICardSet
case class TCard(
  name: String,
  setKey: Option[String] = None,
  manaCost: Option[String] = None,
  number: Option[String] = None,
  image_uris: Option[Images] = None,
  mciNumber: Option[String] = None,
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

  val testData = List(
    TCard(name = "sun", text = Some("sun")),
    TCard(name = "fab lab", text = Some("fab lab")),
    TCard(name = "gun", text = Some("gun")),
    TCard(name = "xxxx sun avatar xxxx", text = Some("xxxx sun avatar xxxx")),
    TCard(name = "yun", text = Some("yun")),
    TCard(name = "sun doo", text = Some("sun doo")),
    TCard(name = "fab lab", text = Some("fab lab")),
    TCard(name = "gun", text = Some("gun")),
    TCard(name = "sun boo", text = Some("sun boo")),
    TCard(name = "fab lab", text = Some("fab lab")),
    TCard(name = "gun", text = Some("gun"))
  )

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "A MagicAct" must {
    "return cards" in {
      val magic   = system.actorOf(Props(classOf[MagicAct]))
      val content = magic ? MagicCards()
      content.onComplete {
        case Success(cards) =>
          cards match {
            case l: List[Any] =>
              l.size should be > 1
            case _ => assert(false)
          }
        // sets match {
        //   case s:List[Card] =>
        //     s.size should be > 1
        //   case _ => assert(false)
        // }
        case Failure(t) =>
          assert(false)
      }
      Await.result(content, 6.seconds)
      assert(content.isCompleted)
    }

    "list scores correctly search w/o punctuation" in {
      val actorRef = TestActorRef(Props(new MagicAct()))
      val magic    = actorRef.underlyingActor.asInstanceOf[MagicAct]
      val s1       = magic.scores("the fox's fur", "fox's")
      val s2       = magic.scores("the fox's fur", "foxs")
      assert(s1 == s2)
    }

    "list scores correctly" in {
      val actorRef = TestActorRef(Props(new MagicAct()))
      val magic    = actorRef.underlyingActor.asInstanceOf[MagicAct]
      val s        = magic.scores("the quick brown fox", "fox")
      assert(s.size > 1)
      assert(s.head == 0)
    }

    "list scores correctly multi input" in {
      val actorRef = TestActorRef(Props(new MagicAct()))
      val magic    = actorRef.underlyingActor.asInstanceOf[MagicAct]
      val s        = magic.scores("sun", "sun avatar")
      assert(s.size == 1)
      assert(s.head == 0)
    }

    "list best match for card" in {
      val actorRef = TestActorRef(Props(new MagicAct()))
      val magic    = actorRef.underlyingActor.asInstanceOf[MagicAct]
      val s        = magic.lookupName("yun", testData)
      assert(s.size == 1)
      assert(s.head.name == "yun")
    }

    "list best match for card grouped on unique name" in {
      val actorRef = TestActorRef(Props(new MagicAct()))
      val magic    = actorRef.underlyingActor.asInstanceOf[MagicAct]
      val s        = magic.lookupName("gun", testData)
      assert(s.size == 1)
      assert(s.head.name == "gun")
    }

    "list best match for card grouped on unique name not exact" in {
      val actorRef = TestActorRef(Props(new MagicAct()))
      val magic    = actorRef.underlyingActor.asInstanceOf[MagicAct]
      val s        = magic.lookupName("gab lab", testData)
      assert(s.size == 1)
      assert(s.head.name == "fab lab")
    }

    "list best match for card with test data" in {
      val actorRef = TestActorRef(Props(new MagicAct()))
      val magic    = actorRef.underlyingActor.asInstanceOf[MagicAct]
      val s        = magic.lookupName("sun avatar", testData)
      assert(s.size == 1)
      assert(s.head.name == "xxxx sun avatar xxxx")
    }

    "list best match for card with real data shivan dragon" in {
      val proxy = TestProbe()
      val tmsg: Message = new Message(
        "1568397986.001300",
        "CML9J1N9G",
        "UM3ETMMPE",
        "card name is shivan dragon?",
        None,
        None
      )
      val cs = CardNameSearch(tmsg, "shivan dragon")
      val parent = system.actorOf(Props(new Actor {
        val child = context.actorOf(Props(classOf[MagicAct]), "child")
        def receive = {
          case x if sender == child =>
            x match {
              case r: Response =>
              // println("==========================")
              // println(r)
              // println("==========================")
              /*
              case n:HipNotif =>
                // println("==========================")
                // println(n)
                // println("==========================")
               */
              case _ => assert(false)
            }
            proxy.ref.forward(x)
          case x => child.forward(x)
        }
      }))

      proxy.send(parent, tmsg)
      proxy.receiveN(1, 8.seconds)
    }

    "list best match for card with real data goblin" in {
      val proxy = TestProbe()
      val tmsg: Message = new Message(
        "1568397986.001300",
        "CML9J1N9G",
        "UM3ETMMPE",
        "card name is goblin?",
        None,
        None
      )

      val cs = CardNameSearch(tmsg, "goblin")
      val parent = system.actorOf(Props(new Actor {
        val child = context.actorOf(Props(classOf[MagicAct]), "child")
        def receive = {
          case x if sender == child =>
            x match {
              case r: Response =>
              // println("==========================")
              // println(r)
              // println("==========================")
              /*
              case n: HipNotif =>
              // println("==========================")
              // println(n)
              // println("==========================")
               */
              case _ => assert(false)
            }
            proxy.ref.forward(x)
          case x => child.forward(x)
        }
      }))

      proxy.send(parent, tmsg)
      proxy.receiveN(1, 8.seconds)
    }

    "list best match for card with real data black lotus" in {
      val proxy = TestProbe()
      val tmsg: Message = new Message(
        "1568397986.001300",
        "CML9J1N9G",
        "UM3ETMMPE",
        "card name is black lotus?",
        None,
        None
      )

      val cs = CardNameSearch(tmsg, "black lotus")
      val parent = system.actorOf(Props(new Actor {
        val child = context.actorOf(Props(classOf[MagicAct]), "child")
        def receive = {
          case x if sender == child =>
            x match {
              case r: Response =>
              // println("==========================")
              // println(r)
              // println("==========================")
              /*
              case n: HipNotif =>
              // println("==========================")
              // println(n)
              // println("==========================")
               */
              case _ => assert(false)
            }
            proxy.ref.forward(x)
          case x => child.forward(x)
        }
      }))

      proxy.send(parent, tmsg)
      proxy.receiveN(1, 8.seconds)
    }

    "list best match for card with real data" in {
      val proxy = TestProbe()
      val tmsg: Message = Message(
        "1568397986.001300",
        "CML9J1N9G",
        "UM3ETMMPE",
        "card name is sun's avatar?",
        None,
        None
      )

      val cs = CardNameSearch(tmsg, "sun's avatar")
      val parent = system.actorOf(Props(new Actor {
        val child = context.actorOf(Props(classOf[MagicAct]), "child")
        def receive = {
          case x if sender == child =>
            x match {
              case r: Response =>
              // println("==========================")
              // println(r)
              // println("==========================")
              /*
              case n: HipNotif =>
              // println("==========================")
              // println(n)
              // println("==========================")
               */
              case _ => assert(false)
            }
            proxy.ref.forward(x)
          case x => child.forward(x)
        }
      }))

      proxy.send(parent, tmsg)
      proxy.receiveN(1, 8.seconds)
    }

    "list best match for card with real data validate no results stub" in {
      val proxy = TestProbe()
      val tmsg: Message = new Message(
        "1568397986.001300",
        "CML9J1N9G",
        "UM3ETMMPE",
        "card name is zcxx?",
        None,
        None
      )
      val parent = system.actorOf(Props(new Actor {
        val child = context.actorOf(Props(classOf[MagicAct]), "child")
        def receive = {
          case x if sender == child =>
            x match {
              case r: Response =>
                println("==========================")
                val pat = """:shrug: your best match was .* with \d?\d.\d\d% and score \d?\d.\d\d%""".r
                pprint.log(r)
                assert(pat.findFirstIn(r.deliver) != None)
                // assert(r == Response(tmsg, "Try asking again with a longer string"))
                println("==========================")
              /*
              case a: HipNotif =>
                println("==========================")
                assert(false)
                println("==========================")
               */
              case _ => assert(false)
            }
            proxy.ref.forward(x)
          case x => child.forward(x)
        }
      }))

      proxy.send(parent, tmsg)
      proxy.receiveN(1, 8.seconds)
    }

    "list best match for card with real data validate no results bad match" in {
      val proxy = TestProbe()
      val res   = "Scuzzback Scrapper"
      val src   = "zcxzzzzzzzzz"
      val tlen  = src.length
      val pcent = (tlen - 9).toFloat / tlen

      val tmsg: Message = Message(
        "1568397986.001300",
        "CML9J1N9G",
        "UM3ETMMPE",
        s"card name is ${src}?",
        None,
        None
      )
      val cs = CardNameSearch(tmsg, s"${src}")
      val parent = system.actorOf(Props(new Actor {
        val child = context.actorOf(Props(classOf[MagicAct]), "child")
        def receive = {
          case x if sender == child =>
            x match {
              case r: Response =>
                println("==========================")
                val jw    = new JaroWinklerDistance()
                val jcent = jw(src, res)
                val p     = "%"
                pprint.log(pcent)
                pprint.log(pcent * 100)
                pprint.log(jcent)
                pprint.log(jcent * 100)
                assert(
                  r.deliver == f""":shrug: your best match was
                                  |${res} with ${pcent * 100}%1.2f$p
                                  |and score ${jcent * 100}%1.2f$p""".stripMargin
                    .replaceAll("\n", " ")
                )
                // Scuzzback Scrapper with 25.00% and score 36.11
                println("==========================")
              /*
              case a:HipNotif =>
                println("==========================")
                assert(false)
                println("==========================")
               */
              case _ => assert(false)
            }
            proxy.ref.forward(x)
          case x => child.forward(x)
        }
      }))

      proxy.send(parent, tmsg)
      proxy.receiveN(1, 8.seconds)
    }


  }
}
