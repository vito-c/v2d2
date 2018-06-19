package v2d2.client.core

import java.lang.System
import java.util.Collection
import org.jxmpp.jid.Jid
import org.jxmpp.jid.impl.JidCreate
import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}
import v2d2.actions.generic.HipUsers

import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.util.Timeout
import org.jivesoftware.smack.StanzaListener
import org.jivesoftware.smack.chat.{Chat, ChatManager, ChatManagerListener, ChatMessageListener}
import org.jivesoftware.smack.packet.{Message, Presence, Stanza}
import org.jivesoftware.smack.roster.{Roster, RosterEntry, RosterListener}
import org.jivesoftware.smack.tcp.XMPPTCPConnection
import org.jivesoftware.smackx.muc.MultiUserChatManager
import org.jivesoftware.smackx.ping.PingManager
import org.jivesoftware.smackx.xhtmlim.XHTMLManager
import org.jxmpp.util.XmppStringUtils
import v2d2.V2D2
import v2d2.actions.generic.HipChatUsersAct
import v2d2.actions.generic._
import v2d2.actions.generic.protocol._
import v2d2.actions.love._
import v2d2.actions.pager._
import v2d2.actions.who._
import v2d2.client._
import v2d2.client.core._
import v2d2.mtg._
import v2d2.parsers._


case class RosterList(roster: List[RosterEntry])
case class Resources(names: List[String])

class RosterActor(connection: XMPPTCPConnection) 
extends Actor 
with ActorLogging {

  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  // implicit val timeout = Timeout(130.seconds)
  implicit val timeout = Timeout(25.seconds)
  val _roster:Roster = Roster.getInstanceFor(connection)
  private var _rosterDirty: Boolean = true
  private var _rosterLoading = false
  private var _rosterCache: List[RosterEntry] = Nil
  // private val profile  = context.actorOf(Props(classOf[ProfileActor], connection), name = "cmd:profile" )

  override def preStart = {
    _roster.addRosterListener(new RosterListener(){
      def entriesAdded(args: Collection[Jid]) = {
        log.info("entires added")
        // _rosterDirty = true
        // _mapsDirty = true
        // TBD
      }
      def entriesDeleted(args: Collection[Jid]) = {
        log.info("entires deleted")
        pprint.log(args, "deleted entries")
        // _rosterDirty = true
        // _mapsDirty = true
        // TBD
      }
      def entriesUpdated(args: Collection[Jid]) = {
        log.info("entires updated")
        // _rosterDirty = true
        // _mapsDirty = true
        // TBD
      }
      def presenceChanged(args: Presence) = {
        // log.info(s"presence changed")
        // _rosterDirty = true
        // _mapsDirty = true
        // TBD
      }
    })
  }

  private def getRoster(): RosterList = {
    if( _rosterDirty == false ) {
      log.info("CACHED ROSTER")
      RosterList(_rosterCache)
    } else {
      _roster.reloadAndWait()
      _rosterDirty = false
      _rosterCache = _roster.getEntries().asScala.toList
      log.info("ROSTER LOADED")
      pprint.log(_rosterCache.take(2), "ROSTER CACHE SAMPLE")
      RosterList(_rosterCache)
    }
  }

  def receive: Receive = {
    // case TestUsers(names) =>
    //   val ns = names.toSet
    //   pprint.log(ns, "name set")
    //   sender() ! RosterList(
    //     _rosterCache filter { e => ns(e.getName()) })

    case Resources(names) =>
      val ns = names.toSet
      pprint.log(ns, "name set")
      sender() ! RosterList(
        _rosterCache filter { e => ns(e.getName()) })

    case MakeRosterDirty() =>
      log.info("dirty roster")
      _rosterDirty = true;

    // case entry: RosterEntry =>
    //   sender() ! (for {
    //     p <- (profile ? ProfileRQ(entry.getUser())).mapTo[Profile]
    //   } yield { 
	// 	User(
    //       name     = entry.getName(),
    //       jid      = entry.getUser(),
    //       nick     = p.mention_name,
    //       email    = p.email,
    //       timezone = Timezone(p.timezone, p.offset),
    //       entry    = entry
    //     ) 
    //   })

    case AcquireRoster(names) =>
      log.info("roster list request")
      if( names != Nil && _rosterCache != Nil ) {
        pprint.log(names, "ROSTER EXISTs AND NAMES EXIST")
        self forward Resources(names)
      } else if(names != Nil) {
        pprint.log(names, "NAMES EXIST ONLY")
        getRoster()
        self forward Resources(names)
      } else {
        // val res = if( _rosterDirty == false ) {
        //   log.info("CACHED ROSTER")
        //   RosterList(_rosterCache.take(2))
        // } else {
        //   _roster.reloadAndWait()
        //   _rosterDirty = false
        //   _rosterCache = _roster.getEntries().asScala.toList
        //   log.info("ROSTER LOADED")
        //   RosterList(_rosterCache.take(2))
        // }
        log.info("DEFAULT GET ROSTER ONLY")
        sender() ! getRoster()
      }
  }

}
