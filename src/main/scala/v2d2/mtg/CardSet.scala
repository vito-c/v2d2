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
// "XLN": {
//   "name": "Ixalan",
//   "code": "XLN",
//   "magicCardsInfoCode": "xln",
//   "releaseDate": "2017-09-29",
//   "border": "black",
//   "type": "expansion",
//   "block": "Ixalan",
//   "mkm_name": "Ixalan",
//   "mkm_id": 114,
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
with CardProtocol
{
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

