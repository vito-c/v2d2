package v2d2.client

import org.jivesoftware.smack.packet.{SimpleIQ, IQ}
import org.jivesoftware.smack.packet.SimpleIQ
import org.jivesoftware.smack.provider.IQProvider
import java.io.IOException
import org.xmlpull.v1.XmlPullParser
import org.xmlpull.v1.XmlPullParserException
import org.jivesoftware.smack.packet.IQ.IQChildElementXmlStringBuilder
import scala.annotation.tailrec
import org.jivesoftware.smack.filter.StanzaFilter
import org.jivesoftware.smack.StanzaListener
import org.jivesoftware.smack.packet.Stanza
import org.jivesoftware.smack.SmackException.NotConnectedException;
import org.jivesoftware.smack.SmackException.NotLoggedInException;


trait IProfile extends SimpleIQ {
  /**************************************************************************************
    <iq id="profile_498F941D-9B4D-4B2E-BED3-31E5A62E83AA"
      type="get"
      to="10804_157983@chat.hipchat.com">
        <query xmlns="http://hipchat.com/protocol/profile"/>
    </iq>
    <iq id="profile_498F941D-9B4D-4B2E-BED3-31E5A62E83AA"
      xmlns="jabber:client"
      from="10804_157983@chat.hipchat.com"
      type="result"
      to="10804_157983@chat.hipchat.com/osx-64051">
        <query xmlns="http://hipchat.com/protocol/profile">
            <email>npetrov@atlassian.com</email>
            <name>Nikolay Petrov</name>
            <mention_name>npe</mention_name>
            <photo_large>https://www.hipchat.com/img/silhouette_125.png</photo_large>
            <photo_small>https://www.hipchat.com/img/silhouette_36.png</photo_small>
            <!-- Timezone is optional element -->
            <timezone utc_offset="-420.0">UTC</timezone>
            <title>Senior Developer</title>
            <skype_name/>
        </query>
    </iq>
  ***************************************************************************************/
}

object ProfileIQ {
  val ELEMENT: String = "query"
  val NAMESPACE: String = "http://hipchat.com/protocol/profile"

  def apply() = new SimpleIQ(ELEMENT, NAMESPACE) with IProfile
  def apply(person: String) = new SimpleIQ(ELEMENT, NAMESPACE) with IProfile {
    setTo(person)
    setType(IQ.Type.get);
  }

  def apply(
    mention_name: String,
    name: String,
    photo_large: String,
    photo_small: String,
    title: String,
    email: String,
    timezone: String,
    offset: Double
  ) = new SimpleIQ(ELEMENT, NAMESPACE) with IProfile
}

case class Profile(
  mention_name: String,
  name: String,
  photo_large: String,
  photo_small: String,
  title: String,
  email: String,
  timezone: String,
  offset: Double
)
object Profile {
  def apply(iq: ProfileIQ): Profile = {
    new Profile(
      iq.mention_name,
      iq.name,
      iq.photo_large,
      iq.photo_small,
      iq.title,
      iq.email,
      iq.timezone,
      iq.offset
    )
  }
}

class ProfileIQ(
  mention_name: String,
  name: String,
  photo_large: String,
  photo_small: String,
  title: String,
  email: String,
  timezone: String,
  offset: Double
) extends SimpleIQ(ProfileIQ.ELEMENT, ProfileIQ.NAMESPACE) with IProfile {
  def mention_name(): String = mention_name
  def name(): String = name
  def photo_large(): String = photo_large
  def photo_small(): String = photo_small
  def title(): String = title
  def email(): String = email
  def timezone(): String = timezone
  def offset(): Double = offset
}


class ProfileFilter extends StanzaFilter {
  override def accept(packet: Stanza):Boolean = {
    //TODO: Implement
    var profileIQReceived: Boolean = false;
    false
  }
}

class ProfileListener extends StanzaListener {
  @throws(classOf[NotConnectedException])
  @throws(classOf[InterruptedException])
  @throws(classOf[NotLoggedInException])
  override def processStanza(packet: Stanza): Unit = {
    //TODO: Implement
  }
}

class ProfileProvider extends IQProvider[ProfileIQ] {
    @throws(classOf[XmlPullParserException])
    @throws(classOf[IOException])
    override def parse(parser: XmlPullParser, initialDepth: Int): ProfileIQ = {
      // println(s"==================================")
      var mention_name : String = ""
      var name         : String = ""
      var photo_large  : String = ""
      var photo_small  : String = ""
      var title        : String = ""
      var email        : String = ""
      var timezone     : String = ""
      var offset       : Double = 0.0d

      @tailrec
      def iterator():Unit = {
        val tag = parser.next()
        // println(s"tag: ${tag}")
        tag match {
          case XmlPullParser.START_TAG => 
            val node = parser.getName()
            // println(s"node: ${node}")
            node match {
              case "mention_name" =>
                mention_name = parser.nextText()
                // println(s"mention_name: ${mention_name}")
                iterator()
              case "name" =>
                name = parser.nextText()
                // println(s"name: ${name}")
                iterator()
              case "photo_large" =>
                photo_large = parser.nextText()
                // println(s"photo_large: ${photo_large}")
                iterator()
              case "photo_small" =>
                photo_small = parser.nextText()
                // println(s"photo_small: ${photo_small}")
                iterator()
              case "title" =>
                title = parser.nextText()
                // println(s"title: ${title}")
                iterator()
              case "email" =>
                email = parser.nextText()
                // println(s"email: ${email}")
                iterator()
              case "timezone" =>
                try {
                  offset = parser.getAttributeValue(null, "utc_offset").toFloat
                } catch {
                  case e: Exception =>
                    println(s"exception: ${e}")
                   // case ioe: IOException => ... // more specific cases first !
                   // case e: Exception => ...
                }
                timezone = parser.nextText()
                // println(s"offset $offset")
                // println(s"timezone: ${timezone}")
                iterator()
              case _ =>
                val skip = parser.nextText()
                // println(s"skipping: ${parser.nextText()}")
                iterator()
            }
          case XmlPullParser.END_TAG =>
            if (parser.getDepth() != initialDepth) {
              iterator()
            }
        }
      }

      iterator()
      // println(s"==================================")

    // No need to use the profile constructor with arguments. IQ will already
    // have filled out all relevant fields ('from', 'to', 'id').
    new ProfileIQ(
      mention_name = mention_name,
      name         = name,
      photo_large  = photo_large,
      photo_small  = photo_small,
      title        = title,
      email        = email,
      timezone     = timezone,
      offset       = offset
    )
  }
}
