package v2d2.actions.generic

import v2d2.actions.generic.protocol._
import org.scalatest._
import akka.testkit.TestActorRef

class LoveSpec extends FlatSpec with Matchers {
  def loveAssert(love:Option[Love], targets: Seq[String], reason: Option[String]) = {
    love should not be (None)
    love match {
      case Some(loves) =>
        loves.targets.toSet should be (targets.toSet)
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
}
