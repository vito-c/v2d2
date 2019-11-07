package v2d2.mtg

import scala.concurrent._
import v2d2.actions.generic.protocol._
import scala.util.{Failure, Success}
import v2d2.actions.love._
import v2d2.actions.who._
import org.scalatest._
import akka.testkit.TestActorRef
import v2d2.client._
import java.io.InputStream
import scala.io.Source
import akka.util.Timeout
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem, Props}
import akka.stream.ActorMaterializer
import scala.concurrent.duration._

class CardNameSearchSpec 
extends FlatSpec 
with Matchers
with CardSetProtocol {
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25.seconds)

  val tmsg: IMessage = new EMessage()
  // Testing CardNameSearch case class for love
  // example: who is tough.guy?
  def cardSearchAssert(
    input: String, 
    target: String, 
    description: String
  ) = {
    val emsg: IMessage = new EMessage()
    val ocs: Option[CardNameSearch] = CardNameSearch(input, emsg)
    input should description in {
      ocs match {
        case Some(cs) =>
          cs.target should be (target)
        case _ => 
          assert(ocs != None)
      }
    }
  }

  cardSearchAssert("v2d2, what card name is foo bar?", "foo bar", "parse foo bar as the target")
  cardSearchAssert("v2d2, what card name is foo bar", "foo bar", "parse foo bar as the target")
  cardSearchAssert("!card name foo bar", "foo bar", "parse foo bar as the target")
  cardSearchAssert("card name foo bar", "foo bar", "parse foo bar as the target")
  cardSearchAssert("cardname foo bar", "foo bar", "parse foo bar as the target")

  cardSearchAssert("v2d2, what card name is foo?", "foo", "parse foo as the target")
  cardSearchAssert("v2d2, what card name is foo", "foo", "parse foo as the target")
  cardSearchAssert("!card name foo", "foo", "parse foo as the target")
  cardSearchAssert("card name foo", "foo", "parse foo as the target")
  cardSearchAssert("cardname foo", "foo", "parse foo as the target")

  cardSearchAssert("v2d2, what card name is sun's avatar?", "sun's avatar", "parse foo as the target")
  cardSearchAssert("v2d2, what card name is sun's avatar", "sun's avatar", "parse foo as the target")
  cardSearchAssert("!card name sun's avatar", "sun's avatar", "parse foo as the target")
  cardSearchAssert("card name sun's avatar", "sun's avatar", "parse foo as the target")
  cardSearchAssert("cardname sun's avatar", "sun's avatar", "parse foo as the target")

  cardSearchAssert("v2d2, what card name is burning sun's avatar?", "burning sun's avatar", "parse foo as the target")
  cardSearchAssert("v2d2, what card name is burning sun's avatar", "burning sun's avatar", "parse foo as the target")
  cardSearchAssert("!card name burning sun's avatar", "burning sun's avatar", "parse foo as the target")
  cardSearchAssert("card name burning sun's avatar", "burning sun's avatar", "parse foo as the target")
  cardSearchAssert("cardname burning sun's avatar", "burning sun's avatar", "parse foo as the target")

  cardSearchAssert("card name isolate", "isolate", "parse isolate as the target")
  cardSearchAssert("card name is isolate", "isolate", "parse isolate as the target")

  // cardSearchAssert("v2d2, what card name is burning sun's avatar?", "burning sun's avatar", "parse foo as the target")
  // cardSearchAssert("v2d2, what card name is burning sun's avatar", "burning sun's avatar", "parse foo as the target")
  // cardSearchAssert("!card name burning sun's avatar", "burning sun's avatar", "parse foo as the target")
  // cardSearchAssert("card name burning sun's avatar", "burning sun's avatar", "parse foo as the target")
  // cardSearchAssert("cardname burning sun's avatar", "burning sun's avatar", "parse foo as the target")

  "v2d2, what card name is" should "parse none" in {
    CardNameSearch("v2d2, what card name is", tmsg) should be (None)
  }
  "v2d2, what card name" should "parse none" in {
    CardNameSearch("v2d2, what card name", tmsg) should be (None)
  }
  "card name" should "parse none" in {
    CardNameSearch("card name", tmsg) should be (None)
  }
  "!card name" should "parse none" in {
    CardNameSearch("!card name", tmsg) should be (None)
  }

  "what card name is " should "parse none" in {
    CardNameSearch("what card name is", tmsg) should be (None)
  }

  "Time to parse the cards to case classes" should "parse correctly" in {
    val stream : InputStream = getClass.getResourceAsStream("/allsets.json")
    val lines = scala.io.Source.fromInputStream(stream).mkString
    // val json = 
    // Source.fromInputStream(
    //   getClass.getResourceAsStream("/allsets.json")
    // ).getLines().mkString("")

    val req:Future[List[Card]] = Unmarshal(lines).to[List[Card]]

    val foo = req.onComplete({
      case Success(cs) => {
        assert(cs.size > 0)
        Some(cs)
      }
      case Failure(exception) => {
        None
      }
    })
    // assert( foo != None)

    Await.result(req, 25.seconds)
  }

  // val test1 = s"v2d2, what card is foo bar?"
  // val tmsg: IMessage = new EMessage()
  // test1 should s"parse foo bar" in {
  //   val ocs = CardNameSearch(test1, tmsg)
  //   ocs should not be (None)
  //   ocs match {
  //     case Some(cs) =>
  //       cs.target should be ("foo bar")
  //     case _ => None
  //   }
  // }
}
 
