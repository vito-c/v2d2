package v2d2

import akka.actor.{ActorRef, Actor, ActorSystem, ActorContext, Props, ActorLogging}
import akka.japi.Util.immutableSeq
import akka.pattern.ask
import akka.util.Timeout
import collection.JavaConversions._
import com.typesafe.config.Config
import com.typesafe.config._
import com.typesafe.scalalogging.Logger
import concurrent.Future
import concurrent.Promise
import fastparse.all._
import fastparse.parsers.Combinators.Rule
import fastparse.parsers._
import java.util.Collection
import org.apache.log4j.ConsoleAppender
import org.apache.log4j.Level
import org.apache.log4j.LogManager
import org.apache.log4j.PatternLayout
import org.jivesoftware.smack.ConnectionConfiguration
import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import org.jivesoftware.smack._
import org.jivesoftware.smack.packet.Presence
import org.jivesoftware.smack.packet.Stanza
import org.jivesoftware.smack.provider.ProviderManager
import org.jivesoftware.smack.roster.{RosterListener,Roster,RosterEntry,RosterLoadedListener}
import org.jivesoftware.smack.tcp.{XMPPTCPConnectionConfiguration, XMPPTCPConnection}
import org.jivesoftware.smackx.muc.Affiliate
import org.jivesoftware.smackx.muc.Occupant
import org.jivesoftware.smackx.muc.{MultiUserChatManager, DiscussionHistory, MultiUserChat}
import org.slf4j.LoggerFactory
import reflect.runtime.universe.Type
import reflect.runtime.universe.Type
import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.concurrent.duration._
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Success, Failure}
import v2d2.client.core.{XMPPActor, JoinRoom}
import v2d2.client.{Profile,ProfileProvider,ProfileIQ,User}
import v2d2.client.core.Ping

object V2D2 extends App with LoggerConfig {

  // Use the system’s dispatcher as ExecutionContext
  import system.dispatcher
  // SmackConfiguration.DEBUG = true;
  val system   = ActorSystem("system")
  log.info("system booting up")

  implicit val timeout = Timeout(5.seconds)

  val vitoJid = "1_492@chat.btf.hipchat.com"
  val v2d2Jid = "1_1821@btf.hipchat.com"

  val conf = ConfigFactory.load("v2d2.conf")
  val creds = ConfigFactory.load("creds.conf")
  val rooms = conf.getList("v2d2.rooms").toList
  val uid: String      = creds.getString("creds.user")
  val port: Int        = creds.getInt("creds.port")
  val host: String     = creds.getString("creds.host")
  val display: String  = creds.getString("creds.display")
  val password: String = creds.getString("creds.password")


  private var _rosterDirty = true
  private var _mapsDirty = true

  private val xconf = XMPPTCPConnectionConfiguration.builder()
    .setServiceName(host)
    .setPort(port)
    .setResource("bot")
    .setSecurityMode(SecurityMode.required)
    .setHost(host).build()
  private val _connection = new XMPPTCPConnection(xconf)
  log.info("xmpptcp connection built")

  try {
    _connection.setPacketReplyTimeout(5000)
    _connection.connect()
    log.info("xmpptcp connection established")
    _connection.login(uid, password)
    log.info("login complete")
  } catch {
    case NonFatal(e) =>
      log.error("Login or connection exception", e)
      if (_connection.isConnected) _connection.disconnect()
  }

  def who(prop: String): String = {
    conf.getString(s"v2d2.who.${prop}")
  }

  def sendLove(prop: String): String = {
    conf.getString(s"v2d2.sendlove.${prop}")
  }

  // _roster.setSubscriptionMode(Roster.SubscriptionMode.accept_all)
  // roster.addRosterListener(new RosterListener(){
  //   def entriesAdded(args: Collection[String]) = {
  //     log.info("entires added")
  //     // _rosterDirty = true
  //     // _mapsDirty = true
  //     // TBD
  //   }
  //   def entriesDeleted(args: Collection[String]) = {
  //     log.info("entires deleted")
  //     // _rosterDirty = true
  //     // _mapsDirty = true
  //     // TBD
  //   }
  //   def entriesUpdated(args: Collection[String]) = {
  //     log.info("entires updated")
  //     // _rosterDirty = true
  //     // _mapsDirty = true
  //     // TBD
  //   }
  //   def presenceChanged(args: Presence) = {
  //     // log.info("presence changed")
  //     // _rosterDirty = true
  //     // _mapsDirty = true
  //     // TBD
  //   }
  // })

  _connection.sendPacket(new Presence(Presence.Type.available))

  // if (!roster.isLoaded()) roster.reload()
  def connection():XMPPTCPConnection = {
    _connection
  }

  // user logged on now add listeners
  ProviderManager.addIQProvider(ProfileIQ.ELEMENT, ProfileIQ.NAMESPACE, new ProfileProvider())


  val xactor = system.actorOf(
    Props(classOf[XMPPActor], _connection),
    name = "xmpp"
  )

  xactor ! Ping()
  val cancellable =
  system.scheduler.schedule(0 milliseconds, 5000 milliseconds) {
    xactor ! Ping()
  }

  // val rooms = ConfigFactory.load().getList("v2d2.rooms").toList
  // rooms map { cv =>
  //   val config = (cv.asInstanceOf[ConfigObject]).toConfig();
  //   val name = config.getString("name")
  //   val pass = config.getString("pass")
  //   xactor ! JoinRoom(name, Some(pass))
  // }

  rooms foreach { entry =>
    val config = (entry.asInstanceOf[ConfigObject]).toConfig();
    val name = config.getString("name")
    val pass = config.getString("pass")
    log.info(s"name: ${name} pass: ${pass}")
    xactor ! JoinRoom(name, Some(pass))
  }

  // val actors = conf.getList("v2d2.actors").toList
  // actors foreach { entry =>
  //   system.log.info(s"entry: ${entry}")
  // }

  // PingManager.getInstanceFor(_connection).setPingInterval(5)
}

trait LoggerConfig {
  val layout = new PatternLayout("%d %5p [%t] - %c - %m%n")
  val consoleAppender = new ConsoleAppender(layout, ConsoleAppender.SYSTEM_OUT)
  val rootLogger = LogManager.getRootLogger
  val log = Logger(LoggerFactory.getLogger("main"))
  rootLogger.setLevel(Level.ALL)
  rootLogger.addAppender(consoleAppender)
}
