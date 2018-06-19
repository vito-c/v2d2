package v2d2.client

import org.jivesoftware.smack.roster.RosterEntry
import org.jxmpp.jid.BareJid
import v2d2.actions.generic.hipchat.HipProfile

case class Timezone(
  zone: String, // UTC, US/Eastern, America/Los_Angeles etc
  offset: Double // the offset from UTC in minutes 240.0, 420.0
)
// jid is set to room jid would be nice to have actual jid
case class User(
  name    :String,
  jid     :BareJid,
  nick    :String,
  email   :String,
  timezone:Timezone,
  entry   :RosterEntry,
  profile :HipProfile
)
