package v2d2.actions.knock

import akka.testkit.TestActorRef
import scala.concurrent.duration._
import scala.concurrent.Await
import v2d2.actions.knock._
import collection.mutable.Stack
import org.scalatest._

class KnockKnockSpec extends FlatSpec with Matchers {

  def knockAssert(knock:Option[KnockKnock], target:String) = {
    knock should not be (None)
    knock match {
      case Some(knocker) =>
      knocker.target match {
        case Some(nick) =>
        nick should be (target)
        case _ => None
      }
      case _ => None
    }
  }

  "!knockknock @vimFTW" should "target vimFTW" in {
    knockAssert( KnockKnock("!knockknock @vimFTW"), "vimFTW")
  }
  "!knock @vimFTW" should "target vimFTW" in {
    knockAssert( KnockKnock("!knock @vimFTW"), "vimFTW")
  }
  "!knock knock @vimFTW" should "target vimFTW" in {
    knockAssert( KnockKnock("!knock knock @vimFTW"), "vimFTW")
  }
  "knock knock @vimFTW SPACE" should "target vimFTW" in {
    knockAssert( KnockKnock("knock knock @vimFTW "), "vimFTW")
  }
  "v2d2 knock knock @vimFTW" should "target vimFTW" in {
    knockAssert( KnockKnock("v2d2 knock knock @vimFTW "), "vimFTW")
  }
  "v2d2, knock knock @vimFTW" should "target vimFTW" in {
    knockAssert( KnockKnock("v2d2, knock knock @vimFTW "), "vimFTW")
  }
  "V2D2, knock knock @vimFTW" should "target vimFTW" in {
    knockAssert( KnockKnock("V2D2, knock knock @vimFTW "), "vimFTW")
  }
  "knock knock failures" should "blah blah should be None" in {
    KnockKnock("blah blah blah") should be (None)
  }

  "whois" should "return Some(Whois(Some(\"string\")))" in {
    Whois("bot, Who is there?") should be (Some(Whois(Some("bot, Who is there?"))))
    Whois("who's there?") should be (Some(Whois(Some("who's there?"))))
    Whois("v2d2, Who's there?") should be (Some(Whois(Some("v2d2, Who's there?"))))
    Whois("v2d2, Who's there??") should be (Some(Whois(Some("v2d2, Who's there??"))))
    Whois("v2d2, who is that!") should be (Some(Whois(Some("v2d2, who is that!"))))
    Whois("v2d2, who is that?") should be (Some(Whois(Some("v2d2, who is that?"))))
    Whois("v2d2, who is that") should be (Some(Whois(Some("v2d2, who is that"))))
    Whois("!who there?") should be (Some(Whois(Some("!who there?"))))
    Whois("who there") should be (Some(Whois(Some("who there"))))
    Whois("who dat") should be (Some(Whois(Some("who dat"))))
    Whois("who that") should be (Some(Whois(Some("who that"))))
    Whois("who (dat)") should be (Some(Whois(Some("who (dat)"))))
    Whois("who is there??") should be (Some(Whois(Some("who is there??"))))
    Whois("blah blah blah") should be (None)
  }

  "who" should "return Some(Who(Some(\"abc who?\")))" in {
    val clue = "abc"
    Who(s"v2d2, ${clue} who?") should be (Some(Who(Some("v2d2, abc"))))
    Who(s"v2d2, ${clue} who??") should be (Some(Who(Some("v2d2, abc"))))
    Who(s"v2d2, ${clue}?") should be (None)
    // Who(s"bot, ${clue} who?")
    // Who(s"!${clue} who?")
    // Who(s"${clue} who?")
    // Who("who?")

  }
  

  // "knockknock" should  {
  //   "fart to B" {
  //     val actorRef = TestActorRef(new Knocker)
  //   }
  // }

}
// // Spec class which extends TestKit
// "A" should {
//   "send the right message to B" {
//     val actorRef = TestActorRef(new Knocker)
//     // hypothetical message stimulating a '42' answer
//     val future = actorRef ? Say42
//     val Success(result: Int) = future.value.get
//     result should be(42)
//   }
// }
