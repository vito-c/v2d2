package v2d2.client

import org.jxmpp.util.XmppStringUtils
import org.jivesoftware.smack.packet.{Presence, Message}
import org.apache.commons.lang3.StringUtils
import akka.actor.ActorRef
import scala.util.matching._
import org.jivesoftware.smackx.muc.MultiUserChat
import akka.actor.ActorSystem
import org.jivesoftware.smackx.xhtmlim.XHTMLText
import org.jivesoftware.smack.util.XmlStringBuilder
import org.jivesoftware.smack.packet.DefaultExtensionElement
import org.jivesoftware.smack.packet.Element

trait IMessage {
  def content:  String = { "v2d2, foo bar baz" }
  def fromJid:  String = { "from you" }
  def fromName: String = { "SomeTestName" } // display name
  def fromNick: String = { "SomeTestName" } // mention name
  def fromRaw:  String = { "from you" }

  // def publish(msg:String): Unit = { println(msg) }
  def fromMsgJid: String = { "blah" }
}

case class MsgData(
  content:  String,
  fromJid:  String,
  fromName: String,
  fromNick: String,
  fromRaw:  String,
  fromMsgJid: String
)

class DMessage(data: MsgData) extends IMessage {
  override def content:     String = { data.content }
  override def fromJid:     String = { data.fromJid }
  override def fromName:    String = { data.fromName }
  override def fromNick:    String = { data.fromNick }
  override def fromRaw:     String = { data.fromRaw }
  override def fromMsgJid:  String = { data.fromMsgJid }
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
  override def content:     String = { xmsg.getBody() }
  override def fromJid:     String = { XmppStringUtils.parseBareJid(xmsg.getFrom()) }
  override def fromName:    String = { XmppStringUtils.parseResource(xmsg.getFrom()) }
  override def fromNick:    String = { "(poo)" }
  override def fromRaw:     String = { xmsg.getFrom() }
  override def fromMsgJid:  String = { xmsg.getFrom() }
}

//MultiUserMessage
class MUMessage(xmsg: Message, chat: MultiUserChat) extends XMessage(xmsg) with IMessage {

  override def fromRaw: String = {
    val occupant = chat.getOccupant(xmsg.getFrom())
    if (occupant != null)
      occupant.getJid()
    else ""
  }

  override def fromJid: String = {
    if (fromRaw != "")
      XmppStringUtils.parseBareJid(fromRaw)
    else ""
  }

  // override def fromMsgJid: String = {
  //   xmsg.getFrom()
  // }
}

// <body>
// 	<p style='font-size:large'>Hey John, this is my new
// 		<span style='color:green'>green</span>
// 		<em>!!!!</em>
// 	</p>
// </body>
// // Create a message to send
// Message msg = chat.createMessage();
// msg.setSubject("Any subject you want");
// msg.setBody("Hey John, this is my new green!!!!");
//
// // Create an XHTMLText to send with the message
// XHTMLText xhtmlText = new XHTMLText(null, null);
// xhtmlText.appendOpenParagraphTag("font-size:large");
// xhtmlText.append("Hey John, this is my new ");
// xhtmlText.appendOpenSpanTag("color:green");
// xhtmlText.append("green");
// xhtmlText.appendCloseSpanTag();
// xhtmlText.appendOpenEmTag();
// xhtmlText.append("!!!!");
// xhtmlText.appendCloseEmTag();
// xhtmlText.appendCloseParagraphTag();
// xhtmlText.appendCloseBodyTag();
//
// // Add the XHTML text to the message
// XHTMLManager.addBody(msg, xhtmlText);

// public XHTMLText appendOpenParagraphTag(String style) {
//     text.halfOpenElement(P);
//     text.optAttribute(STYLE, style);
//     text.rightAngleBracket();
//     return this;
// }
class XHTMLMemo(xhtml: XHTMLText = new XHTMLText(null, null)) {

  def notif() = {
    val ex = new DefaultExtensionElement("x", "http://hipchat.com/protocol/muc#room")
    // ex.setValue("message_format","html")
    // ex.setValue("color","yellow")
    // ex.setValue("type","system")
    // ex.setValue("notify","0")
    ex
  }

  def dump() = {
xhtml.appendOpenParagraphTag("font-size:large");
xhtml.append("Hey John, this is my new ");
xhtml.appendOpenSpanTag("color:green");
xhtml.append("green");
xhtml.appendCloseSpanTag();
xhtml.appendOpenEmTag();
xhtml.append("!!!!");
xhtml.appendCloseEmTag();
xhtml.appendCloseParagraphTag();
xhtml.appendCloseBodyTag();
    // xhtml.toXML().
    //   openElement("p").
    //   escape("hello world").
    //   closeElement("p")
	// xhtml.appendCloseBodyTag()
  } 
}
