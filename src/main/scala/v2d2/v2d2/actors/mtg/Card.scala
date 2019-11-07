package v2d2.mtg

import spray.json._
import fastparse._
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
//
//
// new format
// "object": "card",
// "id": "fefbf149-f988-4f8b-9f53-56f5878116a6",
// "oracle_id": "711eea87-0fa3-46e0-a42b-fa5a86455f04",
// "multiverse_ids": [
//   222
// ],
// "name": "Shivan Dragon",
// "lang": "en",
// "uri": "https://api.scryfall.com/cards/fefbf149-f988-4f8b-9f53-56f5878116a6",
// "scryfall_uri": "https://scryfall.com/card/lea/174/shivan-dragon?utm_source=api",
// "layout": "normal",
// "highres_image": true,
// "image_uris": {
//   "small": "https://img.scryfall.com/cards/small/en/lea/174.jpg?1525123073",
//   "normal": "https://img.scryfall.com/cards/normal/en/lea/174.jpg?1525123073",
//   "large": "https://img.scryfall.com/cards/large/en/lea/174.jpg?1525123073",
//   "png": "https://img.scryfall.com/cards/png/en/lea/174.png?1525123073",
//   "art_crop": "https://img.scryfall.com/cards/art_crop/en/lea/174.jpg?1525123073",
//   "border_crop": "https://img.scryfall.com/cards/border_crop/en/lea/174.jpg?1525123073"
// },
// "mana_cost": "{4}{R}{R}",
// "cmc": 6,
// "type_line": "Creature — Dragon",
// "oracle_text": "Flying (This creature can't be blocked except by creatures with flying or reach.)\n{R}: Shivan Dragon gets +1/+0 until end of turn.",
// "power": "5",
// "toughness": "5",
// "colors": [
//   "R"
// ],
// "color_identity": [
//   "R"
// ],
// "legalities": {
//   "standard": "legal",
//   "future": "legal",
//   "frontier": "legal",
//   "modern": "legal",
//   "legacy": "legal",
//   "pauper": "not_legal",
//   "vintage": "legal",
//   "penny": "legal",
//   "commander": "legal",
//   "1v1": "legal",
//   "duel": "legal",
//   "brawl": "legal"
// },
// "reserved": false,
// "foil": false,
// "nonfoil": true,
// "oversized": false,
// "reprint": false,
// "set": "lea",
// "set_name": "Limited Edition Alpha",
// "set_uri": "https://api.scryfall.com/sets/lea",
// "set_search_uri": "https://api.scryfall.com/cards/search?order=set&q=e%3Alea&unique=prints",
// "scryfall_set_uri": "https://scryfall.com/sets/lea?utm_source=api",
// "rulings_uri": "https://api.scryfall.com/cards/fefbf149-f988-4f8b-9f53-56f5878116a6/rulings",
// "prints_search_uri": "https://api.scryfall.com/cards/search?order=set&q=%21%E2%80%9CShivan+Dragon%E2%80%9D+include%3Aextras&unique=prints",
// "collector_number": "174",
// "digital": false,
// "rarity": "rare",
// "flavor_text": "While it's true most Dragons are cruel, the Shivan Dragon seems to take particular glee in the misery of others, often tormenting its victims much like a cat plays with a mouse before delivering the final blow.",
// "illustration_id": "d326c884-fed0-4b92-bd88-fd4989597c20",
// "artist": "Melissa A. Benson",
// "frame": "1993",
// "full_art": false,
// "border_color": "black",
// "timeshifted": false,
// "colorshifted": false,
// "futureshifted": false,
// "story_spotlight": false,
// "edhrec_rank": 5535,
// "related_uris": {
//   "gatherer": "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=222",
//   "tcgplayer_decks": "http://decks.tcgplayer.com/magic/deck/search?contains=Shivan+Dragon&page=1&partner=Scryfall",
//   "edhrec": "http://edhrec.com/route/?cc=Shivan+Dragon",
//   "mtgtop8": "http://mtgtop8.com/search?MD_check=1&SB_check=1&cards=Shivan+Dragon"
//  }

case class ImageUris(
  small: String,
  normal: String,
  large: String,
  png: String,
  art_crop: String,
  border_crop: String
)

// case class OldCard(
//   // ctype: String,
//   artist: String,
//   cmc: Option[Int], // Converted Mana Cost
//   colorIdentity: Option[Seq[String]],
//   colors: Option[Seq[String]],
//   flavor: Option[String],
//   id: String,
//   layout: String,
//   manaCost: Option[String],
//   mciNumber: Option[String],
//   multiverseid: Option[Int],
//   name: String,
//   number: Option[String],
//   power: Option[String],
//   rarity: String,
//   setKey: Option[String],
//   subtypes: Option[Seq[String]],
//   text: Option[String],
//   toughness: Option[String],
//   types: Option[Seq[String]]
// ) extends ICard

case class Images(
  small: String,
  normal: String,
  large: String,
  png: String,
  art_crop: String,
  border_crop: String
)
case class Legalities(
  standard: String,
  future: String,
  frontier: Option[String],
  modern: String,
  legacy: String,
  pauper: String,
  vintage: String,
  penny: String,
  commander: String,
  // 1v1: String,
  duel: String,
  brawl: String
)
case class Card(
  cmc: Option[Int], // Converted Mana Cost
  colorIdentity: Option[Seq[String]],
  colors: Option[Seq[String]],
  flavor: Option[String],
  id: String,
  image_uris: Option[Images],
  layout: String,
  legalities: Legalities,
  manaCost: Option[String],
  mciNumber: Option[String],
  name: String,
  number: Option[String],
  power: Option[String],
  rarity: String,
  scryfall_uri: String,
  set: String,
  setKey: Option[String],
  subtypes: Option[Seq[String]],
  text: Option[String],
  toughness: Option[String],
  types: Option[Seq[String]],
  uri: String
) extends ICard

trait CardLegalitiesProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val CardLegalitiesFormat = jsonFormat11(Legalities.apply)
}

trait CardImageProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val CardImagesFormat = jsonFormat6(Images.apply)
}

trait CardProtocol
  extends SprayJsonSupport
  with DefaultJsonProtocol
  with CardLegalitiesProtocol
  with CardImageProtocol {
  // implicit val CardLegalitiesFormat = jsonFormat11(Legalities.apply)
  // implicit val CardImagesFormat = jsonFormat6(Images.apply)
  implicit val CardFormat = jsonFormat22(Card.apply)

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

trait ICard {
  def name: String
  def setKey: Option[String]
  def number: Option[String]
  def text: Option[String]
  def manaCost: Option[String]
  def mciNumber: Option[String]
  def image_uris: Option[Images]
}

trait ICardSet {
  def name: String
  def cards: List[ICard]
}
case class CardSet(
  block: Option[String],
  border: String,
  cards: List[Card],
  code: String,
  magicCardsInfoCode: Option[String],
  mkm_id: Option[Int],
  mkm_name: Option[String],
  name: String
  // releaseDate: DateTime,
  // ctype: String
) extends ICardSet

trait CardSetProtocol
  extends SprayJsonSupport
  with DefaultJsonProtocol
// with CardLegalitiesProtocol
// with CardImageProtocol
  with CardProtocol {
  // implicit val CardLegalitiesFormat = jsonFormat11(Legalities.apply)
  // implicit val CardImagesFormat = jsonFormat6(Images.apply)
  // implicit val CardFormat = jsonFormat22(Card.apply)
  implicit val CardSetFormat = jsonFormat8(CardSet.apply)
  //
  // implicit val CardFormat = jsonFormat(
  //   Card.apply,
  //   "artist",
  //   "cmc",
  //   "colorIdentity",
  //   "colors",
  //   "flavor"
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
  // implicit val CardSetFormat = jsonFormat(
  //   CardSet.apply,
  //   "block",
  //   "border",
  //   "cards",
  //   "code",
  //   "magicCardsInfoCode",
  //   "mkm_id",
  //   "mkm_name",
  //   "name"
  //   // "releaseDate",
  //   // "type"
  // )
}
