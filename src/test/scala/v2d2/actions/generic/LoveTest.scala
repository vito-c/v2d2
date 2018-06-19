package v2d2.actions.generic

import v2d2.actions.generic.protocol._
import v2d2.actions.love._
import v2d2.actions.who._
import org.scalatest._
import akka.testkit.TestActorRef
import v2d2.client._

class LoveSpec extends FlatSpec with Matchers {
  val tmsg: IMessage = new EMessage()
  def whoLoveAssert(whoLove:Option[WhoLove], target: String) = {
    whoLove should not be (None)
    whoLove match {
      case Some(whoLove) =>
        whoLove.target should be (target)
      case _ => None
    }
  }

  def whoDoLoveAssert(whodo:Option[WhoDoLove], target: String) = {
    whodo should not be (None)
    whodo match {
      case Some(whodo) =>
        whodo.target should be (target)
      case _ => None
    }
  }
  def loveAssert(love:Option[Love], targets: Seq[String], reason: Option[String]) = {
    love should not be (None)
    love match {
      case Some(loves) =>
        loves.targets.toSet should be (targets.toSet)
      case _ => None
    }
  }

  "love failures" should "blah blah should be None" in {
    Love("blah blah blah") should be (None)
  }

  val reason = Some("any intresting reason")
  val p1 = "vimFTW"
  val p2 = "abc"
  val cmd = "love"

  val test1 = s"!${cmd} ${reason.getOrElse("")} @${p1} @${p2}" 
  test1 should s"parse targets ${p1} ${p2} and reason: ${reason.getOrElse("")}" in {
    loveAssert( 
      Love(test1), 
      Seq(p1,p2),
      reason
    )
  }

  val test2 = s"bot, ${cmd} ${reason.getOrElse("")} @${p1} @${p2}" 
  test2 should s"parse targets ${p1} ${p2} and reason: ${reason.getOrElse("")}" in {
    loveAssert( 
      Love(test2), 
      Seq(p1,p2),
      reason
    )
  }

  val test3 = s"v2d2, ${cmd} ${reason.getOrElse("")} @${p1} @${p2}" 
  test3 should s"parse targets ${p1} ${p2} and reason: ${reason.getOrElse("")}" in {
    loveAssert( 
      Love(test3), 
      Seq(p1,p2),
      reason
    )
  }

  val test4 = s"v2d2, ${cmd} @${p1} @${p2} ${reason.getOrElse("")} " 
  test4 should s"parse targets ${p1} ${p2} and reason: ${reason.getOrElse("")}" in {
    loveAssert( 
      Love(test4),
      Seq(p1,p2),
      reason
    )
  }

  val test5 = s"V2D2, love you are so cool @${p1} @${p2}"
  test5 should "parse targets: vimFTW abc and reason: you are so cool" in {
    loveAssert( 
      Love(test5), 
      Seq(p1,p2),
      Some("you are so cool")
    )
  }

  val test6 = s"love you are so cool @${p1} @${p2}"
  test6 should "parse targets: vimFTW abc and reason: you are so cool" in {
    loveAssert( 
      Love(test6), 
      Seq(p1,p2),
      Some("you are so cool")
    )
  }

  val test7 = s"love @${p1} great jokes"
  test7 should "parse targets: vimFTW and reason: great jokes" in {
    loveAssert( 
      Love(test7), 
      Seq(p1),
      Some("you are so cool")
    )
  }
  val test8 = s"love great jokes @${p1}"
  test8 should "parse targets: vimFTW and reason: great jokes" in {
    loveAssert( 
      Love(test8), 
      Seq(p1),
      Some("you are so cool")
    )
  }

  // who love test
  // who loves me, who loves @Foo
  val test9 = s"who loves me"
  test9 should "parse me as the target" in {
    whoLoveAssert( 
      WhoLove(test9, tmsg), 
      "me"
    )
  }

  val test10 = s"who loves me?"
  test10 should "parse me as the target" in {
    whoLoveAssert( 
      WhoLove(test10, tmsg), 
      "me"
    )
  }

  val test11 = s"who loves @Foo"
  test11 should "parse Foo as the target" in {
    whoLoveAssert( 
      WhoLove(test11, tmsg), 
      "Foo"
    )
  }

  val test12 = s"who loves @Foo ?"
  test12 should "parse Foo as the target" in {
    whoLoveAssert( 
      WhoLove(test12, tmsg), 
      "Foo"
    )
  }
  val test13 = s"who loves @Foo?"
  test13 should "parse Foo as the target" in {
    whoLoveAssert( 
      WhoLove(test13, tmsg), 
      "Foo"
    )
  }

  // Who do test love
  // Who do I love, who does @Foo love
  val test14 = s"who do i love"
  test14 should "parse i as the target" in {
    whoDoLoveAssert( 
      WhoDoLove(test14, tmsg), 
      "i"
    )
  }

  val test15 = s"who do i love?"
  test15 should "parse i as the target with question" in {
    whoDoLoveAssert( 
      WhoDoLove(test15, tmsg), 
      "i"
    )
  }
  
  val test16 = s"who does @Foo love"
  test16 should "parse Foo as the target" in {
    whoDoLoveAssert( 
      WhoDoLove(test16, tmsg), 
      "Foo"
    )
  }

  val test17 = s"who does @Foo love?"
  test17 should "parse Foo as the target with question" in {
    whoDoLoveAssert( 
      WhoDoLove(test17, tmsg), 
      "Foo"
    )
  }

  val test18 = s"who does @Foo rove?"
  test18 should "parse None" in {
    WhoDoLove(test18, tmsg) should be (None)
  }

  val test19 = s"who\tdoes\t@Foo\tlove?"
  test19 should "parse Foo as the target with question" in {
    whoDoLoveAssert( 
      WhoDoLove(test19, tmsg), 
      "Foo"
    )
  }


  // Testing WhoIs case class for love
  // example: who is tough.guy?
  def whoIsAssert(
    input: String, 
    target: String, 
    description: String
  ) = {
    val emsg: IMessage = new EMessage()
    val whoIs:Option[WhoIs] = WhoIs(input, emsg)

    input should description in {
      whoIs match {
        case Some(who) =>
          who.target should be (target)
        case _ => 
          assert(whoIs != None)
      }
    }
  }

  whoIsAssert("who is yo.mama", "yo.mama", "parse yo.mama as the target")
  whoIsAssert("who is yo.mama?", "yo.mama", "parse yo.mama as the target as question")
  whoIsAssert("who is @SomeCoolGuy", "SomeCoolGuy", "parse SomeCoolGuy as the target")
  whoIsAssert("who is @vimFTW\u2620", "vimFTW\u2620", "parse vimFTW\u2620 as the target")
  whoIsAssert("who is @vimFTW☠", "vimFTW\u2620", "parse vimFTW\u2620 as unicode char as the target")
  whoIsAssert("who is @SomeCoolGuy?", "SomeCoolGuy", "parse SomeCoolGuy as the target as question")
  whoIsAssert("who is @SomeCoolGuy???", "SomeCoolGuy", "parse SomeCoolGuy as the target as question")
  whoIsAssert("who is @SomeCoolGuy ?", "SomeCoolGuy", "parse SomeCoolGuy as the target as question with space")
  whoIsAssert("who is @Some1CoolGuy ", "Some1CoolGuy", "parse Some1CoolGuy as the target as question with space")
  whoIsAssert("who is yo.mama ?", "yo.mama", "parse yo.mama as the target as question with space")
  whoIsAssert("who is yo.mama@rallyhealth.com", "yo.mama", "parse yo.mama as the target as question with space")
  whoIsAssert("who is Someone Cool", "Someone Cool", "parse Someone Cool as the target")
  whoIsAssert("who is Someone Cool?", "Someone Cool", "parse Someone Cool as the target as question")
  whoIsAssert("who is Someone Cool ?", "Someone Cool", "parse Someone Cool as the target as question with space")
  "who is it" should "parse none" in {
    WhoIs("who is it", tmsg) should be (None)
  }
  "who is their" should "parse none" in {
    WhoIs("who is their", tmsg) should be (None)
  }
  "who is there" should "parse none" in {
    WhoIs("who is there", tmsg) should be (None)
  }
  "who is on call" should "parse none" in {
    WhoIs("who is on call", tmsg) should be (None)
  }

}
