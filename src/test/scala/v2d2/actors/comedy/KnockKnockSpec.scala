package v2d2.actors.comedy

import akka.testkit.TestActorRef
import scala.concurrent.duration._
import scala.concurrent.Await
import org.scalatest._
import slack.models.Message

class KnockKnockSpec extends FlatSpec with Matchers {

// v2d2.actors.core.MuxActor#receive $anonfun#applyOrElse:23 msg: Message(
//   "1568397986.001300",
//   "CML9J1N9G",
//   "UM3ETMMPE",
//   "<@UMZMZHU2D> will only say hi if he is online <@ULMEYUZRU> though he can be choosey <@UN2S9KRBR>",
//   None,
//   None
// )
//
  val tmsg: Message = new Message(
    "1568397986.001300",
    "CML9J1N9G",
    "UM3ETMMPE",
    "",
    None,
    None
  )

  def knockAssert(
    knock: Option[KnockKnock],
    target: String
  ) = {
    knock should not be (None)
    knock match {
      case Some(knocker) =>
        knocker.target match {
          case Some(nick) =>
            nick should be(target)
          case _ => None
        }
      case _ => None
    }
  }

  def knockItAssert(
    knock: Option[Knockit]
  ) = {
    knock should not be (None)
    knock match {
      case Some(knocker) =>
      // knocker.target match {
      //   case Some(nick) =>
      //     nick should be(target)
      //   case _ => None
      // }
      case _ => None
    }
  }

  "knock knock" should "not knockit" in {
    Knockit("v2d2, knock knock <@vimFTW>", tmsg) should be(None)
  }

  "knock knock" should "knockit" in {
    knockItAssert(Knockit("v2d2, knock knock", tmsg))
  }

  "!knockknock @vimFTW" should "target vimFTW" in {
    knockAssert(KnockKnock("!knockknock <@vimFTW>", tmsg), "vimFTW")
  }
  "!knock @vimFTW" should "target vimFTW" in {
    knockAssert(KnockKnock("!knock <@vimFTW>", tmsg), "vimFTW")
  }
  "!knock knock @vimFTW" should "target vimFTW" in {
    knockAssert(KnockKnock("!knock knock <@vimFTW>", tmsg), "vimFTW")
  }
  "knock knock @vimFTW SPACE" should "target vimFTW" in {
    knockAssert(KnockKnock("knock knock <@vimFTW> ", tmsg), "vimFTW")
  }
  "v2d2 knock knock @vimFTW" should "target vimFTW" in {
    knockAssert(KnockKnock("v2d2 knock knock <@vimFTW> ", tmsg), "vimFTW")
  }
  "v2d2, knock knock @vimFTW" should "target vimFTW" in {
    knockAssert(KnockKnock("v2d2, knock knock <@vimFTW> ", tmsg), "vimFTW")
  }
  "V2D2, knock knock @vimFTW" should "target vimFTW" in {
    knockAssert(KnockKnock("V2D2, knock knock <@vimFTW> ", tmsg), "vimFTW")
  }
  "knock knock failures" should "blah blah should be None" in {
    KnockKnock("blah blah blah", tmsg) should be(None)
  }

  "whois" should "return tmsg, Some(Whois(Some(\"string\")))" in {
    Whois("bot, WHO IS THERE", tmsg) should be(Some(Whois(tmsg, Some("bot, WHO IS THERE"))))
    Whois("WHO IS THERE", tmsg) should be(Some(Whois(tmsg, Some("WHO IS THERE"))))
    Whois("who's there", tmsg) should be(Some(Whois(tmsg, Some("who's there"))))
    Whois("bot, who’s there?", tmsg) should be(Some(Whois(tmsg, Some("bot, who’s there?"))))
    Whois("bot, Who is there?", tmsg) should be(Some(Whois(tmsg, Some("bot, Who is there?"))))
    Whois("who's there?", tmsg) should be(Some(Whois(tmsg, Some("who's there?"))))
    Whois("v2d2, Who's there?", tmsg) should be(Some(Whois(tmsg, Some("v2d2, Who's there?"))))
    Whois("v2d2, Who's there??", tmsg) should be(Some(Whois(tmsg, Some("v2d2, Who's there??"))))
    Whois("v2d2, who is that!", tmsg) should be(Some(Whois(tmsg, Some("v2d2, who is that!"))))
    Whois("v2d2, who is that?", tmsg) should be(Some(Whois(tmsg, Some("v2d2, who is that?"))))
    Whois("v2d2, who is that", tmsg) should be(Some(Whois(tmsg, Some("v2d2, who is that"))))
    Whois("!who there?", tmsg) should be(Some(Whois(tmsg, Some("!who there?"))))
    Whois("who there", tmsg) should be(Some(Whois(tmsg, Some("who there"))))
    Whois("who dat", tmsg) should be(Some(Whois(tmsg, Some("who dat"))))
    Whois("who that", tmsg) should be(Some(Whois(tmsg, Some("who that"))))
    Whois("who (dat)", tmsg) should be(Some(Whois(tmsg, Some("who (dat)"))))
    Whois("who is there??", tmsg) should be(Some(Whois(tmsg, Some("who is there??"))))
    Whois("blah blah blah", tmsg) should be(None)
  }

  "who" should "return Some(Who(Some(\"abc who?\")))" in {
    val clue = "abc"
    Who(s"who's there", tmsg) should be(None)
    Who(s"v2d2, ${clue} who?", tmsg) should be(Some(Who(tmsg, Some("v2d2, abc"))))
    Who(s"v2d2, ${clue} who??", tmsg) should be(Some(Who(tmsg, Some("v2d2, abc"))))
    Who(s"v2d2, ${clue}?", tmsg) should be(None)
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
