package v2d2.client

import org.jivesoftware.smack.roster.RosterEntry

// jid is set to room jid would be nice to have actual jid
case class User(
  name     : String,
  jid      : String,
  nick     : String,
  email    : String,
  entry    : RosterEntry
)
