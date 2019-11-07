package v2d2.actors.comedy

import org.apache.commons.text.similarity._
import akka.actor.ActorSystem
import v2d2.protocols.Response
import akka.testkit.{ImplicitSender, TestActorRef, TestActors, TestKit}
import org.scalatest.{BeforeAndAfterAll}
import slack.models.Message
import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import akka.testkit.TestProbe
import akka.actor.ActorRef
import akka.testkit.TestFSMRef
import akka.pattern.ask
import org.scalatest.AsyncWordSpecLike
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers._

// import org.mockito.{Matchers, Mockito}
// import org.mockito.Matchers._
//
class KnockActSpec
  extends TestKit(ActorSystem("KnockActSpec"))
  with ImplicitSender
  with MockitoSugar
  with AsyncWordSpecLike
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import system.dispatcher
  implicit val timeout = Timeout(25.seconds)

  def target = "ABC"
  def from   = "sender"

  def sendmsg(
    str: String
  ): Message = {
    Message(
      "1568397986.001300",
      "CML9J1N9G",
      from,
      str,
      None,
      None
    )
  }

  def whodat: Message = {
    Message(
      "1568397986.001300",
      "CML9J1N9G",
      target,
      "who is there?",
      None,
      None
    )
  }

  def whowho(
    str: String
  ): Message = {
    Message(
      "1568397986.001300",
      "CML9J1N9G",
      target,
      s"${str} who?",
      None,
      None
    )
  }

  "A KnockKnock Joke" must {

    "start a joke with a mark response" in {
      val knocker = system.actorOf(Props(classOf[Knocker]), "knock")
      val msg     = sendmsg(s"knock knock <@${target}>")
      (knocker ? msg).mapTo[Response].map { k =>
        assert(k.deliver == s"""Knock, knock <@${target}>!""")
      }
    }

    "test the whole joke" in {
      val probe   = TestProbe()
      val knocker = system.actorOf(Props(classOf[Knocker]), "knock1")
      val msg     = sendmsg(s"knock knock <@${target}>")
      val future  = (knocker ? msg).mapTo[Response]
      future.flatMap { kr =>
        assert(kr.deliver == s"""Knock, knock <@${target}>!""")
        (knocker ? whodat).mapTo[Response].flatMap { wr =>
          val clue  = wr.deliver.replaceAll(".*, ", "")
          val index = Clues.clues.indexOf(clue)
          assert(Clues.clues.contains(clue))

          (knocker ? whowho(clue)).mapTo[Response].map { wwr =>
            val ans = wwr.deliver.replaceFirst("<@[^>]*>, ", "").split(s"\n")(0)
            pprint.log(wwr)
            assert(Answers.answers.contains(ans))
          }
        }
      }
    }

    "test the jokes on cpu" in {
      val probe   = TestProbe()
      val knocker = system.actorOf(Props(classOf[Knocker]), "knock2")
      val msg     = sendmsg(s"v2d2, knock knock")
      val future  = (knocker ? msg).mapTo[Response]
      future.flatMap { kr =>
        val ans0 = kr.deliver.replaceFirst("<@[^>]*> ", "")
        pprint.log(ans0)
        assert(ans0 == s"""<@${msg.user}>, who's there?""")
        (knocker ? sendmsg(s"v2d2, Art")).mapTo[Response].flatMap { wr =>
          val ans1 = wr.deliver
          pprint.log(ans1)
          assert(ans1 == "<@sender> Art who?")
          (knocker ? sendmsg(s"R2-D2")).mapTo[Response].map { wwr =>
            val ans2 = wwr.deliver.replaceFirst("<@[^>]*> ", "")
            pprint.log(ans2)
            assert(ans2 == "Ha Ha Ha. You humans are so funny!")
          }
        }
      }
    }

    "test the jokes on cpu call back" in {
      val probe   = TestProbe()
      val knocker = system.actorOf(Props(classOf[Knocker]), "knock2")
      val msg     = sendmsg(s"v2d2, knock knock")
      val future  = (knocker ? msg).mapTo[Response]
      future.flatMap { kr =>
        val ans0 = kr.deliver.replaceFirst("<@[^>]*> ", "")
        pprint.log(ans0)
        assert(ans0 == s"""<@${msg.user}>, who's there?""")
        (knocker ? sendmsg(s"v2d2, Art")).mapTo[Response].flatMap { wr =>
          val ans1 = wr.deliver
          pprint.log(ans1)
          assert(ans1 == "<@sender> Art who?")
          (knocker ? sendmsg(s"R2-D2")).mapTo[Response].map { wwr =>
            val ans2 = wwr.deliver.replaceFirst("<@[^>]*> ", "")
            pprint.log(ans2)
            assert(ans2 == "Ha Ha Ha. You humans are so funny!")
          }
        }
      }
    }

  }
}
// trait BaseV2D2Fixture2 {
//
//   def afterAll {
//     TestKit.shutdownActorSystem(system)
//   }
//
//   def msg: Message = {
//     Message(
//       "1568397986.001300",
//       "CML9J1N9G",
//       from,
//       message,
//       None,
//       None
//     )
//   }
//
//
//   val proxy = TestProbe()
//   def from = "sender"
//   def message = "message"
//   def matcher(c:ActorRef): PartialFunction[Any,Unit] = {
//     case _ => assert("" == "override this method and test your msgs")
//   }
//
// }
//
// trait KnockFixture extends BaseV2D2Fixture2 {
//
//   def target  = "ABC"
//
//   def name: String
//   lazy val knocker = system.actorOf(Props(classOf[Knocker]), name)
//
//   def handleResponse(r:Response, c:ActorRef) = { println("no op") }
//   def handleTargets(ts:Targets) = { println("no op") }
//   def handleResponses(rs:Responses) = { println("no op") }
//
//   override def matcher(c:ActorRef): PartialFunction[Any,Unit] = {
//     case r:Response =>
//       handleResponse(r,c)
//
//     case t:Targets =>
//       handleTargets(t)
//
//     case t:Responses =>
//       handleResponses(t)
//   }
// }

// "start a joke with a mark" in new KnockFixture {
//
//   override def name = "knock0"
//   override def message = s"knock knock <@${target}>"
//
//   override def handleResponse(r:Response, c:ActorRef) = {
//     pprint.log(r)
//     assert(true == false)
//     assert(r.deliver == s"""Knock, knock <@${target}>!""")
//   }
//
//   override def handleTargets(t:Targets) = {
//     assert(true == false)
//     assert(target != t.trgs.getOrElse(target, Joke("error", "error")).target)
//     assert(from != t.trgs.getOrElse(target, Joke("error", "error")).sender)
//   }
//
//   proxy.send(knocker, msg)
//   proxy.send(knocker, GetTargets())
//   //
//   // proxy.send(knocker, msg)
//   // proxy.expectMsg(Response(received = msg, deliver = s"""Knock, knock <@${target}>!"""))
//   proxy.receiveN(2, 8.seconds)
// }
//
// "execute whole joke" in new KnockFixture {
//
//   override def message = s"knock knock <@${target}>"
//   override def name = "knock1"
//
//   val whodat = Message(
//     "1568397986.001300",
//     "CML9J1N9G",
//     from,
//     s"who's there?",
//     None,
//     None
//   )
//
//   val who = Message(
//     "1568397986.001300",
//     "CML9J1N9G",
//     from,
//     s"who?",
//     None,
//     None
//   )
//
//
//   override def handleResponse(r:Response, c:ActorRef) = {
//     if(r.deliver == s"""Knock, knock <@${target}>!"""){
//       assert(r.deliver == s"""Knock, knock <@${target}>!""")
//     } else {
//       pprint.log(r.deliver)
//       assert(r.deliver == s"""FART FART""")
//     }
//   }
//
//   override def handleTargets(t:Targets) = {
//     // assert(target != t.trgs.getOrElse(target, Joke("error", "error")).target)
//     // assert(from != t.trgs.getOrElse(target, Joke("error", "error")).sender)
//   }
//
//   // proxy.send(knocker, GetTargets())
//   proxy.send(knocker, msg)
//   proxy.send(knocker, whodat)
//   // proxy.send(knocker, who)
//   proxy.receiveN(2, 8.seconds)
// }
