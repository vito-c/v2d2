package v2d2.actions.pager

import org.scalatest._
import akka.testkit.TestActorRef
import v2d2.client._

class PagerSpec extends FlatSpec with Matchers {

  // Testing OnCall case class
  // example: bot, who is on call 
  def onCallAssert(
    input: String, 
    team: Option[String], 
    date: Option[String], 
    description: String
  ) = {
    val onCall:Option[OnCall] = OnCall(input)

    input should description in {
      onCall match {
        case Some(call) =>
          call.team should be (team)
          call.date should be (date)
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
  onCallAssert("on call for myteam", Some("myteam"), None, "parse myteam as the team name")
  onCallAssert("on call for myteam today", Some("myteam"), Some("today"), "parse today as the date")
  onCallAssert("on call for myteam on Janurary 2nd 2017", Some("myteam"), Some("Janurary 2nd 2017"), "parse Janurary 2nd 2017 as the date")
  onCallAssert("who is on call for myteam", Some("myteam"), None, "parse myteam as the team name")
  onCallAssert("who is on call for myteam today", Some("myteam"), Some("today"), "parse today as the date")
  onCallAssert("who is on call for myteam on Janurary 2nd 2017", Some("myteam"), Some("Janurary 2nd 2017"), "parse Janurary 2nd 2017 as the date")

  onCallTeamsAssert("what are the on call teams?", None, "parse an on call team class")
  onCallTeamsAssert("on call teams?", None, "parse an on call team class")

  onCallTeamsAssert("foo on call team?", Some(List("foo")), "parse Some(List(\"foo\"))")
  onCallTeamsAssert("is foo on call team?", Some(List("foo")), "parse Some(List(\"foo\"))")
  onCallTeamsAssert("foo an on call team?", Some(List("foo")), "parse Some(List(\"foo\"))")
  onCallTeamsAssert("is foo an on call team?", Some(List("foo")), "parse Some(List(\"foo\"))")
  onCallTeamsAssert("is foo an on call teams?", Some(List("foo")), "parse Some(List(\"foo\"))")

  onCallTeamsAssert("are foo, baz on call teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  onCallTeamsAssert("foo, baz on call teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  onCallTeamsAssert("foo, baz, bar on call teams?", Some(List("foo", "baz", "bar")), "parse Some(List(\"foo\", \"baz\", \"bar\"))")
  onCallTeamsAssert("foo,baz,bar on call teams?", Some(List("foo", "baz", "bar")), "parse Some(List(\"foo\", \"baz\", \"bar\"))")

  onCallTeamsAssert("are foo and baz on call teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  onCallTeamsAssert("are foo, and baz on call teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  onCallTeamsAssert("are foo, bar, and baz on call teams?", Some(List("foo", "bar", "baz")), "parse Some(List(\"foo\", \"bar\", \"baz\"))")
  onCallTeamsAssert("are foo and baz oncall teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  onCallTeamsAssert("are foo, and baz oncall teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  onCallTeamsAssert("are foo, bar, and baz oncall teams?", Some(List("foo", "bar", "baz")), "parse Some(List(\"foo\", \"bar\", \"baz\"))")

  onCallTeamsAssert("are foo and baz pagerduty teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  onCallTeamsAssert("are foo, and baz pagerduty teams?", Some(List("foo", "baz")), "parse Some(List(\"foo\", \"baz\"))")
  onCallTeamsAssert("are foo, bar, and baz pagerduty teams?", Some(List("foo", "bar", "baz")), "parse Some(List(\"foo\", \"bar\", \"baz\"))")

  
  "what are the poogerduty teams?" should "parse none" in {
    OnCallTeams("what are the poogerduty teams?", tmsg) should be (None)
  }

  "poogerduty teams?" should "parse none" in {
    OnCallTeams("poogerduty teams?", tmsg) should be (None)
  }

  "an call teams?" should "parse none" in {
    OnCallTeams("an call teams?", tmsg) should be (None)
  }
}
