package v2d2.client

import org.jxmpp.util.XmppStringUtils
import org.jivesoftware.smack.packet.{Presence, Message}
import org.apache.commons.lang3.StringUtils
import akka.actor.ActorRef
import scala.util.matching._

case class Memo(to: String, from: String, body: String)
trait IMessage {
  def content:  String = { "v2d2, foo bar baz" }
  def fromJid:  String = { "from you" }
  def fromName: String = { "SomeTestName" } // display name
  def fromNick: String = { "SomeTestName" } // mention name
  def fromRaw:  String = { "from you" }

  def publish(msg:String): Unit = { println(msg) }
}

class EMessage extends IMessage {
}

class XMessage(xmsg: Message) extends IMessage {
  // override def senderId: String = {
  //   val frm = xmsg.getFrom()
  //   println(s"from: ${frm}")
  //   println(s"resr: ${XmppStringUtils.parseResource(xmsg.getFrom())}")
  //   println(s"jidr: ${XmppStringUtils.parseBareJid(xmsg.getFrom())}")
  //   XmppStringUtils.parseBareJid(xmsg.getFrom())
  // }
  override def content:  String = { xmsg.getBody() }
  override def fromJid:  String = { XmppStringUtils.parseBareJid(xmsg.getFrom()) }
  override def fromName: String = { XmppStringUtils.parseResource(xmsg.getFrom()) }
  override def fromNick: String = { "(poo)" }
  override def fromRaw:  String = { xmsg.getFrom() }
}
