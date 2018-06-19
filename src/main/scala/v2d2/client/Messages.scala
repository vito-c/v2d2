package v2d2.client

import org.jxmpp.jid.Jid
import org.jxmpp.jid.impl.JidCreate
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
  def fromJid:  Jid = { JidCreate.from("vito@vito-test.com") }
  def fromName: String = { "SomeTestName" } // display name
  def fromNick: String = { "SomeTestName" } // mention name
  def fromRaw:  Option[Jid] = { Some(JidCreate.from("vito@vito-test.com")) }

  // def publish(msg:String): Unit = { println(msg) }
  def fromMsgJid: String = { "blah" }
}

case class MsgData(
  content:  String,
  fromJid:  Jid,
  fromName: String,
  fromNick: String,
  fromRaw:  Option[Jid],
  fromMsgJid: String
)

class DMessage(data: MsgData) extends IMessage {
  override def content:     String = { data.content }
  override def fromJid:     Jid = { data.fromJid }
  override def fromName:    String = { data.fromName }
  override def fromNick:    String = { data.fromNick }
  override def fromRaw:     Option[Jid] = { data.fromRaw }
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
  override def fromJid:     Jid = { pprint.log(xmsg.getFrom(), "XMSG GET FROM"); xmsg.getFrom() }
  override def fromName:    String = { XmppStringUtils.parseResource(xmsg.getFrom().toString()) }
  override def fromNick:    String = { "(poo)" }
  override def fromRaw:     Option[Jid] = { Some(xmsg.getFrom()) }
  override def fromMsgJid:  String = { xmsg.getFrom().toString() }
}

//MultiUserMessage
class MUMessage(xmsg: Message, chat: MultiUserChat) extends XMessage(xmsg) with IMessage {

  override def fromRaw: Option[Jid] = {
    if (xmsg.getFrom().isEntityFullJid()){
      val fulljid = xmsg.getFrom().asEntityFullJidIfPossible()
      val occupant = chat.getOccupant(fulljid)
      if( occupant != null && fulljid != null) {
        return Some(chat.getOccupant(fulljid).getJid()) 
      }
    }
    None
  }

  // TODO: make this an option??
  override def fromJid: Jid = {
    fromRaw match {
      case Some(jid) => jid
      case None => JidCreate.from("uglyhack@causethis_shouldbean_option.com") 
    }
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
case class XHTMLResponse(originalMsg: IMessage, response: XHTMLMemo)
class XHTMLMemo(xhtml: XHTMLText = new XHTMLText(null, null)) {

  def notif() = {
    val ex = new DefaultExtensionElement("x", "http://hipchat.com/protocol/muc#room")
    // ex.setValue("message_format","html")
    // ex.setValue("color","yellow")
    // ex.setValue("type","system")
    // ex.setValue("notify","0")
    ex
  }

  def appedPreText(html: XHTMLText, text: String): XHTMLText = {
    html.appendOpenParagraphTag("")
    html.toXML()
      .halfOpenElement("pre")
      .rightAngleBracket()
      .escape(text)
      .closeElement("pre")
    html.appendCloseParagraphTag()
    html.appendCloseBodyTag();
  }

  def appendOpenPreTag(html: XHTMLText): XHTMLText = {
    html.toXML()
      .halfOpenElement("pre")
      .rightAngleBracket()
    html
  }

  def closePreTag(html:XHTMLText): XHTMLText = {
    html.toXML().closeElement("pre")
    html
  }

  def dump() = {
    val xh = appedPreText(xhtml, "actual html")
    println(s"xh: ${xh}")
    xh

// xhtml.appendOpenParagraphTag("font-size:large");
// xhtml.append("my new test");
// xhtml.appendCloseParagraphTag();
// xhtml.appendCloseBodyTag();

// xhtml.appendOpenSpanTag("color:green");
// xhtml.append("green");
// xhtml.appendCloseSpanTag();
// xhtml.appendCloseSpanTag();
// xhtml.appendOpenEmTag();
// xhtml.append("!!!!");
// xhtml.appendCloseEmTag();
// xhtml.appendCloseParagraphTag();
// xhtml.appendCloseBodyTag();
    // xhtml.toXML().
    //   openElement("p").
    //   escape("hello world").
    //   closeElement("p")
	// xhtml.appendCloseBodyTag()
  } 
}



// [info] [INFO] [08/28/2017 00:07:03.133] [system-akka.actor.default-dispatcher-10] [akka://system/user/xmpp/1_spam_room@conf.btf.hipchat.com] 
// <message id='2xl3o-1313'><body>hello world</body><html xmlns='http://jabber.org/protocol/xhtml-im'><body xmlns='http://www.w3.org/1999/xhtml'><p style='font-size:large'><span style='font-family:menlo'>Hey John, this is my new <span style='color:green'>green</span></span><em>!!!!</em></p></body></html></message>
//
// [info] 	xml: <message to='1_1821@chat.btf.hipchat.com/bot||proxy|hipchat.rallyhealth.com|5252' from='1_spam_room@conf.btf.hipchat.com/testtoken' type='groupchat'><body>&lt;pre&gt;test notif&lt;/pre&gt;</body><x xmlns='http://hipchat.com/protocol/muc#room'><message_format>html</message_format><color>purple</color><type>system</type><notify>0</notify></x><html xmlns='http://jabber.org/protocol/xhtml-im'><body xmlns='http://www.w3.org/1999/xhtml'><pre>test notif</pre></body></html></message>
