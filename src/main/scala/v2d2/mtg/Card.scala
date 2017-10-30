package v2d2.mtg

import spray.json._
import fastparse._
import fastparse.core.Parsed
import fastparse.all._
import fastparse.parsers.Combinators.Rule
import fastparse.parsers._
import v2d2.client.IMessage
import v2d2.parsers.AutoParser
import v2d2.parsers.{Blackspace,BotCombinators}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport

// "artist": "Anna Steinbauer",
// "cmc": 2,
// "colorIdentity": [ "W" ],
// "colors": [ "White" ],
// "id": "ccbc0aca09ec754dd5dc97081315dfdff1748132",
// "imageName": "adanto vanguard",
// "layout": "normal",
// "manaCost": "{1}{W}",
// "mciNumber": "1",
// "multiverseid": 435152,
// "name": "Adanto Vanguard",
// "number": "1",
// "power": "1",
// "rarity": "Uncommon",
// "subtypes": [ "Vampire", "Soldier" ],
// "text": "As long as Adanto Vanguard is attacking, it gets +2/+0.\nPay 4 life: Adanto Vanguard gains indestructible until end of turn. (Damage and effects that say \"destroy\" don't destroy it.)",
// "toughness": "1",
// "type": "Creature — Vampire Soldier",
// "types": [ "Creature" ]
case class Card(

  artist: String,
  cmc: Option[Int], // Converted Mana Cost
  colorIdentity: Option[Seq[String]],
  colors: Option[Seq[String]],
  flavor: Option[String],
  id: String,
  imageName: String,
  layout: String,
  manaCost: Option[String],
  mciNumber: Option[String],
  multiverseid: Option[Int],
  name: String,
  number: Option[String],
  power: Option[String],
  rarity: String,
  subtypes: Option[Seq[String]],
  text: Option[String],
  toughness: Option[String],
  // ctype: String,
  types: Option[Seq[String]],
  setKey: Option[String]
) extends ICard

trait CardProtocol
extends SprayJsonSupport
with DefaultJsonProtocol {
  implicit val CardFormat = jsonFormat20(Card.apply)

  // implicit val CardFormat = jsonFormat(
  //   Card.apply, 
  //   "artist",
  //   "cmc",
  //   "colorIdentity",
  //   "colors",
  //   "id",
  //   "imageName",
  //   "layout",
  //   "manaCost",
  //   "mciNumber",
  //   "multiverseid",
  //   "name",
  //   "number",
  //   "power",
  //   "rarity",
  //   "subtypes",
  //   "text",
  //   "toughness",
  //   // "type",
  //   "types"
  // )
}

