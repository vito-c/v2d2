package v2d2.actions.pager

import org.scalatest._
import akka.testkit.TestActorRef
import v2d2.client._

class PagerSpec extends FlatSpec with Matchers {
  case class TMsgData(content:String)
  class TMessage(data: TMsgData) extends IMessage {
    override def content: String = { data.content }
  }

  // Testing OnCall case class
  // example: bot, who is on call 
  def whenOnCallAssert(
    input: String, 
    target: String, 
    description: String
  ) = {
    val tmsg: IMessage = new TMessage(TMsgData(input))
    val onCall:Option[WhenOnCall] = WhenOnCall(tmsg)

    input should description in {
      onCall match {
        case Some(call) =>
          call.target should be (target)
        case _ => 
          assert(onCall != None)
      }
    }
  }

  def onCallTeamsAssert(
    input: String, 
    teams: Option[List[String]],
    // output: Option[OncallTeams],
    description: String
  ) = {
    val emsg: IMessage = new EMessage()
    val onCall:Option[OnCallTeams] = OnCallTeams(input, emsg)

    input should description in {
      onCall match {
        case Some(oncall) =>
          oncall.teams should be (teams)
        case _ =>
          assert(onCall != None)
      }
    }
  }

  val tmsg: IMessage = new EMessage()
  // onCallAssert("who is on call", "yo.mama", "parse yo.mama as the target")
  // onCallAssert("who is on call right now", "yo.mama", "parse yo.mama as the target")
  // onCallAssert("who is on call today", "yo.mama", "parse yo.mama as the target")
  // onCallAssert("who is on call for myteam", "myteam", "parse my team as the team name")
  //
  whenOnCallAssert("when am i on call", "i", "parse i as the target")
  whenOnCallAssert("when is @foobar on call", "@foobar", "parse foobar as the target")
  whenOnCallAssert("when is @f on call", "@f", "parse f as the target")
  whenOnCallAssert("on call foo.bar@rallyhealth.com", "foo.bar@rallyhealth.com", "parse email for target")
  whenOnCallAssert("when is Foo Bar on call", "Foo Bar", "parse Foo Bar as the target")
  whenOnCallAssert("when is Foo on call", "Foo", "parse Fox as the target")
  whenOnCallAssert("on call Foo Bar", "Foo Bar", "parse Foo Bar as the target")
  whenOnCallAssert("on call foo.bar", "foo.bar", "parse foo.bar as the target")

  // onCallTeamsAssert("what are the on call teams?", None, "parse an on call team class")
  // onCallTeamsAssert("on call teams?", None, "parse an on call team class")
  //
  // onCallTeamsAssert("foo on call team?", Some(List("foo")), "parse Some(List(\"foo\"))")
  // onCallTeamsAssert("is foo on call team?", Some(List("foo")), "parse Some(List(\"foo\"))")
  // onCallTeamsAssert("foo an on call team?", Some(List("foo")), "parse Some(List(\"foo\"))")
  // onCallTeamsAssert("is foo an on call team?", Some(List("foo")), "parse Some(List(\"foo\"))")
  // onCallTeamsAssert("is foo an on call teams?", Some(List("foo")), "parse Some(List(\"foo\"))")
  //
  // onCallTeamsAssert("are foo, baz on call teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  // onCallTeamsAssert("foo, baz on call teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  // onCallTeamsAssert("foo, baz, bar on call teams?", Some(List("foo", "baz", "bar")), "parse Some(List(\"foo\", \"baz\", \"bar\"))")
  // onCallTeamsAssert("foo,baz,bar on call teams?", Some(List("foo", "baz", "bar")), "parse Some(List(\"foo\", \"baz\", \"bar\"))")
  //
  // onCallTeamsAssert("are foo and baz on call teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  // onCallTeamsAssert("are foo, and baz on call teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  // onCallTeamsAssert("are foo, bar, and baz on call teams?", Some(List("foo", "bar", "baz")), "parse Some(List(\"foo\", \"bar\", \"baz\"))")
  // onCallTeamsAssert("are foo and baz oncall teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  // onCallTeamsAssert("are foo, and baz oncall teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  // onCallTeamsAssert("are foo, bar, and baz oncall teams?", Some(List("foo", "bar", "baz")), "parse Some(List(\"foo\", \"bar\", \"baz\"))")
  //
  // onCallTeamsAssert("are foo and baz pagerduty teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  // onCallTeamsAssert("are foo, and baz pagerduty teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  // onCallTeamsAssert("are foo, bar, and baz pagerduty teams?", Some(List("foo", "bar", "baz")), "parse Some(List(\"foo\", \"bar\", \"baz\"))")
  //
  //
  // "what are the poogerduty teams?" should "parse none" in {
  //   OnCallTeams("what are the poogerduty teams?", tmsg) should be (None)
  // }
  //
  // "poogerduty teams?" should "parse none" in {
  //   OnCallTeams("poogerduty teams?", tmsg) should be (None)
  // }
  //
  // "an call teams?" should "parse none" in {
  //   OnCallTeams("an call teams?", tmsg) should be (None)
  // }
}
