package v2d2

import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.control.NonFatal
import org.jxmpp.jid.impl.JidCreate
import v2d2.actions.generic.HipUsersReq

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import com.typesafe.config._
import com.typesafe.scalalogging.Logger
import org.apache.log4j.{ConsoleAppender, Level, LogManager, PatternLayout}
import org.jivesoftware.smack._
import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import org.jivesoftware.smack.packet.Presence
import org.jivesoftware.smack.provider.ProviderManager
import org.jivesoftware.smack.tcp.{XMPPTCPConnection, XMPPTCPConnectionConfiguration}
import org.slf4j.LoggerFactory
import v2d2.client.{ProfileIQ, ProfileProvider}
import v2d2.client.core.{UserList,JoinRoom, Ping, XMPPActor}

object V2D2 
  extends App 
  with LoggerConfig {

  // Use the system’s dispatcher as ExecutionContext
  import system.dispatcher
  // SmackConfiguration.DEBUG = true;
  val system   = ActorSystem("system")
  log.info("system booting up")

  implicit val timeout = Timeout(25.seconds)

  // val vitoJid = "1_492@chat.btf.hipchat.com"
  // val v2d2Jid = "1_1821@btf.hipchat.com"

  val conf = ConfigFactory.load("v2d2.conf")
  val creds = ConfigFactory.load("creds.conf")
  val rooms = conf.getList("v2d2.rooms").toList
  val deployment = conf.getString("v2d2.deployment")

  val uid: String       = creds.getString("creds.user")
  val port: Int         = creds.getInt("creds.port")
  val host: String      = creds.getString("creds.host")
  val display: String   = creds.getString("creds.display")
  val password: String  = creds.getString("creds.password")
  val v2d2Jid: String   = creds.getString("creds.jid")
  val roomToken: String = creds.getString("creds.tokens.room")
  val hcapi: String     = creds.getString("creds.tokens.user")

  private var _rosterDirty = true
  private var _mapsDirty = true

    // .setServiceName(host)
  private val xconf = XMPPTCPConnectionConfiguration.builder()
    .setServiceName(JidCreate.domainBareFrom(host))
    .setPort(port)
    .setResource("bot")
    .setSecurityMode(SecurityMode.required)
    .setHost(host).build()
  private val _connection = new XMPPTCPConnection(xconf)
  log.info("xmpptcp connection built")

  try {
    // This was causing an issue with profile IQ
    _connection.setPacketReplyTimeout(190000)
    _connection.connect()
    log.info("xmpptcp connection established")
    _connection.login(uid, password)
    log.info("login complete")
  } catch {
    case NonFatal(e) =>
      log.error("Login or connection exception", e)
      if (_connection.isConnected) _connection.disconnect()
  }

  def dev(): Boolean = {
    if (deployment == "development") true else false
  }

  def who(prop: String): String = {
    conf.getString(s"v2d2.who.${prop}")
  }

  def sendLove(prop: String): String = {
    conf.getString(s"v2d2.sendlove.${prop}")
  }

  def token(prop: String): String = {
    creds.getString(s"creds.tokens.${prop}")
  }

  def pagerduty(prop: String): String = {
    conf.getString(s"v2d2.pagerduty.${prop}")
  }

  // TBD OPTIMIZE THIS
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

  xactor ! HipUsersReq()

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
  xactor ! UserList()

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
