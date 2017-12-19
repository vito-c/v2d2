package v2d2.parsers

import fastparse._
import fastparse.all._
import fastparse.core.Parsed
import spray.json.DefaultJsonProtocol
import v2d2.client.{IMessage, User}
import v2d2.client.core.ISearchable

// full name: Foo Bar
// last/first name: Foo
// email: foo.bar@rallyhealth.com
// hipchat: @foobar
case class FindUser(needle: String) extends BotCombinators {

  def search: Option[ISearchable] = {
    P((ajid | aemail | anicky | afname | auname | aname).?).parse(needle) match {
      case Parsed.Success(value,  _) => value
      case _ => None
    }
  }
}
