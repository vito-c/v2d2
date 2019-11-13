package v2d2.actions.generic

import v2d2.actions.love._
import v2d2.actions.who._
import org.scalatest._
import akka.testkit.TestActorRef
import slack.models.Message
import fastparse._, SingleLineWhitespace._

class LoveSpec extends FlatSpec with Matchers {

  val tmsg: Message = Message(
    ts = "ts",
    channel = "channel",
    user = "user",
    text = "test",
    is_starred = None,
    thread_ts = None
  )

  def whoLoveAssert(
    whoLove: Option[WhoLove],
    target: String
  ) = {
    whoLove should not be (None)
    whoLove match {
      case Some(whoLove) =>
        whoLove.target should be(target)
      case _ => None
    }
  }

  def whoDoLoveAssert(
    whodo: Option[WhoDoLove],
    target: String
  ) = {
    whodo should not be (None)
    whodo match {
      case Some(whodo) =>
        whodo.target should be(target)
      case _ => None
    }
  }

  def loveAssert(
    love: Option[Love],
    targets: Seq[String],
    reason: Option[String]
  ) = {
    love should not be (None)
    love match {
      case Some(loves) =>
        loves.targets.toSet should be(targets.toSet)
      case _ => None
    }
  }

  def lovingAssert(
    input: String,
    targets: Seq[String],
    reason: String
  ) = {
    val emsg: Message = Message(
      ts = "ts",
      channel = "channel",
      user = "user",
      text = input,
      is_starred = None,
      thread_ts = None
    )
    val love: Option[Love] = Love(input, emsg)

    "love tests" should input in {
      love match {
        case Some(l) =>
          assert(l.reason == Some(reason))
          assert(l.targets.toSet == targets.toSet)
          assert(l.targets.size == targets.size)
        case _ =>
          assert(love != None)
      }
    }
  }

  val str0 = s"crush a super compeling reason <@abc> <@def>!"
  "crush manual test" should str0 in {
    parse(str0, Crush.opt1("crush")(_)) match {
      case Parsed.Success(value, _) =>
        assert(value._2.size == 2)
        assert(true == true)
      case x =>
        pprint.log(x)
        assert(true == false)
    }
  }

  val str1 = s"crush <@abc> <@def> a super compeling reason!"
  "crush manual test" should str1 in {
    parse(str1, Crush.opt2("crush")(_)) match {
      case Parsed.Success(value, _) =>
        assert(value._1.size == 2)
        assert(true == true)
      case x =>
        pprint.log(x)
        assert(true == false)
    }
  }

  "crush manual test creation" should str0 in {
    val out = Crush(
      Message(
        ts = "ts",
        channel = "channel",
        user = "user",
        text = str0,
        is_starred = None,
        thread_ts = None
      )
    )
    assert(out != None)
  }

  val str2 = s"love a super compeling reason <@abc> <@def>!"
  "love manual test" should str2 in {

    parse(str2, Love.opt1("love")(_)) match {
      case Parsed.Success(value, _) =>
        assert(value._2.size == 2)
        assert(true == true)

      case _ =>
        assert(true == false)
    }
  }

  val str3 = s"love <@abc> <@def> a super compeling reason!"
  "love manual test" should str3 in {

    parse(str3, Love.opt2("love")(_)) match {
      case Parsed.Success(value, _) =>
        assert(value._1.size == 2)
        assert(true == true)

      case _ =>
        assert(true == false)
    }
  }

  "love failures" should "blah blah should be None" in {
    Love(tmsg.copy(text = "blah blah blah")) should be(None)
  }

  lovingAssert(
    input = s"!love a super compeling reason <@abc> <@def>",
    targets = List("abc", "def"),
    reason = "a super compeling reason"
  )

  lovingAssert(
    input = s"love a super compeling reason <@abc> <@def>",
    targets = List("abc", "def"),
    reason = "a super compeling reason"
  )

  lovingAssert(
    input = s"love <@abc> a super compeling reason",
    targets = List("abc"),
    reason = "a super compeling reason"
  )

  lovingAssert(
    input = s"!love <@abc> <@def> a super compeling reason",
    targets = List("abc", "def"),
    reason = "a super compeling reason"
  )

  lovingAssert(
    input = s"love <@abc> <@def> a super compeling reason",
    targets = List("abc", "def"),
    reason = "a super compeling reason"
  )

  lovingAssert(
    input = s"i love you <@abc> a super compeling reason",
    targets = List("abc"),
    reason = "a super compeling reason"
  )

  lovingAssert(
    input = s"i love you <@abc> <@def> a super compeling reason",
    targets = List("abc", "def"),
    reason = "a super compeling reason"
  )

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

  val test11 = s"who loves <@Foo>"
  test11 should "parse Foo as the target" in {
    whoLoveAssert(
      WhoLove(test11, tmsg),
      "Foo"
    )
  }

  val test12 = s"who loves <@Foo> ?"
  test12 should "parse Foo as the target" in {
    whoLoveAssert(
      WhoLove(test12, tmsg),
      "Foo"
    )
  }
  val test13 = s"who loves <@Foo>?"
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

  val test16 = s"who does <@Foo> love"
  test16 should "parse Foo as the target" in {
    whoDoLoveAssert(
      WhoDoLove(test16, tmsg),
      "Foo"
    )
  }

  val test17 = s"who does <@Foo> love?"
  test17 should "parse Foo as the target with question" in {
    whoDoLoveAssert(
      WhoDoLove(test17, tmsg),
      "Foo"
    )
  }

  val test18 = s"who does <@Foo> rove?"
  test18 should "parse None" in {
    WhoDoLove(test18, tmsg) should be(None)
  }

  val test19 = s"who\tdoes\t<@Foo>\tlove?"
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
    val emsg: Message = Message(
      ts = "ts",
      channel = "channel",
      user = "user",
      text = "test",
      is_starred = None,
      thread_ts = None
    )
    val whoIs: Option[WhoIs] = WhoIs(input, emsg)

    input should description in {
      whoIs match {
        case Some(who) =>
          who.target should be(target)
        case _ =>
          assert(whoIs != None)
      }
    }
  }

  whoIsAssert("who is yo.mama", "yo.mama", "parse yo.mama as the target")
  whoIsAssert("rallywho is yo.mama", "yo.mama", "parse yo.mama as the target")
  whoIsAssert("rally who is yo.mama", "yo.mama", "parse yo.mama as the target")
  whoIsAssert("who is yo.mama?", "yo.mama", "parse yo.mama as the target as question")
  whoIsAssert("who is <@SomeCoolGuy>", "SomeCoolGuy", "parse SomeCoolGuy as the target")
  whoIsAssert("who is <@vimFTW2620>", "vimFTW2620", "parse vimFTW2620 as the target")
  whoIsAssert("rallywho is <@vimFTW>", "vimFTW", "parse vimFTW as unicode char as the target")
  whoIsAssert("rally who is <@vimFTW>", "vimFTW", "parse vimFTW as unicode char as the target")
  whoIsAssert(
    "who is <@SomeCoolGuy>?",
    "SomeCoolGuy",
    "parse SomeCoolGuy as the target as question"
  )
  whoIsAssert(
    "who is <@SomeCoolGuy>???",
    "SomeCoolGuy",
    "parse SomeCoolGuy as the target as question"
  )
  whoIsAssert(
    "who is <@SomeCoolGuy> ?",
    "SomeCoolGuy",
    "parse SomeCoolGuy as the target as question with space"
  )
  whoIsAssert(
    "who is <@Some1CoolGuy> ",
    "Some1CoolGuy",
    "parse Some1CoolGuy as the target as question with space"
  )
  whoIsAssert("who is yo.mama ?", "yo.mama", "parse yo.mama as the target as question with space")
  whoIsAssert(
    "who is yo.mama@rallyhealth.com",
    "yo.mama@rallyhealth.com",
    "parse yo.mama as the target as question with space"
  )
  whoIsAssert("who is Someone Cool", "Someone Cool", "parse Someone Cool as the target")
  whoIsAssert(
    "who is Someone Cool?",
    "Someone Cool",
    "parse Someone Cool as the target as question"
  )
  whoIsAssert(
    "who is Someone Cool ?",
    "Someone Cool",
    "parse Someone Cool as the target as question with space"
  )
  "who is it" should "parse none" in {
    WhoIs("who is it", tmsg) should be(None)
  }
  "who is their" should "parse none" in {
    WhoIs("who is their", tmsg) should be(None)
  }
  "who is there" should "parse none" in {
    WhoIs("who is there", tmsg) should be(None)
  }
  "who is on call" should "parse none" in {
    WhoIs("who is on call", tmsg) should be(None)
  }

}
