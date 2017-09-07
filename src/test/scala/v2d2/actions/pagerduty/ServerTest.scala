package v2d2.actions.generic.protocol

import org.scalatest._
import akka.testkit.TestActorRef
import v2d2.client._

class ServerSpec extends FlatSpec with Matchers {

  // Testing Server case class
  // example: bot, server rally-demo
  def onServerAssert(
    input: String, 
    servers: Seq[String],
    description: String
  ) = {
    val server:Option[Server] = Server(input)

    input should description in {
      server match {
        case Some(s) =>
          s.servers should be (servers)
        case _ => 
          assert(server != None)
      }
    }
  }

  onServerAssert("servers rally-demo", Seq("rally-demo"), "parse a seq of srings with rally-demo in it")
}

