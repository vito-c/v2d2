package v2d2.mtg

import java.io.InputStream

import scala.collection.immutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.pattern.{ask, pipe}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import org.apache.commons.lang3.StringUtils
import v2d2.actions.generic.HipNotif
import v2d2.actions.generic.protocol.Response
import v2d2.client.IMessage
import v2d2.client.core._

class MagicAct extends Actor with ActorLogging
with CardSetProtocol {
  import system.dispatcher
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(25.seconds)

  val stream : InputStream = getClass.getResourceAsStream("/allsets.json")
  val json = scala.io.Source.fromInputStream(stream).mkString

  def scores(str:String, search:String):List[Int] = {
    val p = s"(\\.|\\*|${"""!<'_-&^%$#@=>,'"""".flatMap(s => s + "|")})"
    str.replaceAll(p,"").toLowerCase().split(" ").map { w => 
      search.replaceAll(p,"").split(" ").map{s =>
        StringUtils.getLevenshteinDistance(w, s)
    }.sorted.head}.toList.sorted
  }

  def lookupName(
    search: String,
    sets: Map[String,ICardSet]
  ): List[ICard] = {
    val cards = sets.values.map(s => s.cards).flatten
    cards.filter(c => c.name.equalsIgnoreCase(search)) match {
      case Nil =>
        val minList = cards.groupBy(
          c => scores(c.name, search).min
        ).toList.sortBy(_._1).head._2
        val endList = minList.groupBy(
          c => scores(c.name, search)
            .groupBy(identity)
            .mapValues(_.size)
            .toList.sortBy(_._1).head
        ).toList.sortBy(_._1)
        val tList = endList.last._2.groupBy(c => c.name).map(t => t._2.head)
        endList.last._2.groupBy(c => c.name).map(t => t._2.head).toList
      case c => 
        Tuple2(0, c)._2.groupBy(c => c.name).map(t => t._2.head).toList
    }
  }

  def receive: Receive = {
    case mc: MagicCards =>
      val req = for {
        sets <- Unmarshal(json).to[Map[String,CardSet]]
        } yield(
          sets.map { t => 
            t._1 -> t._2.copy( cards = t._2.cards map { c => 
              c.copy(setKey = t._2.magicCardsInfoCode) 
            })
          }
        ) 
      req pipeTo sender

    
    case cs:CardNameSearch =>
      val content = for {
        cards <- (self ? MagicCards()).mapTo[Map[String,CardSet]]
      } yield cards
      content onComplete {
        case Success(cards) =>
          val results = lookupName(cs.target.toLowerCase(), cards)
          // if (scores(results.head.name, cs.target.toLowerCase())) {
          // }
          val uri = "https://magiccards.info/scans/en/"
          val imgs = results collect {
            case c if(c.number != None && c.setKey != None) => c
          } map { c =>
            (uri + c.setKey.get.toLowerCase() + "/" + c.number.get + ".jpg" -> c)
          }

          imgs map { t => println(t._1) }
          if (imgs.length > 16) {
            log.info("send one")
            context.parent ! Response(
              cs.imsg, imgs.map { t =>
                s"${t._2.name}: ${t._1}"
              }.mkString("\n"))
          } else {
            log.info("send many")
            val h = if(imgs.size>4) 256 else 321

            val tds = imgs.map( e => 
                s"""<td><img src="${e._1}" height="${h}"</td>""".stripMargin)
            val body = for( (td, i) <- tds.view.zipWithIndex ) yield {
              val j = i + 1
              if ( j % 4 == 0 || j == imgs.size) { s"${td}</tr>" }
              else if( j % 4 == 1) { s"<tr>${td}" }
              else if ( j % 4 > 1 ) { td }
              else td
            }

            val o = s"<table>${body.mkString("")}</table>"

            context.parent ! HipNotif("gray","html",o)
            if (false) { //TODO: keep this so you can add text request
              for((t,i) <- imgs.view.zipWithIndex) {
                system.scheduler.scheduleOnce(500*i milliseconds) {
                  val s = s"""<table height="321">
                  |<tr height="321">
                  |<td><img src="${t._1}" width="225" height="321"></td>
                  |<td height="321"><strong>${t._2.name}</strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;${t._2.manaCost.getOrElse("Land")}<BR>
                  |${t._2.text.getOrElse("").split(" ").zipWithIndex.map { s:(String,Int) => if((s._2+1) % 8 == 0) { s._1 + "<BR>"} else {s._1} }.mkString(" ")}</td>
                  |</tr></table>""".stripMargin
                  println(s)
                  context.parent ! HipNotif("gray","html",s)
                }
              }
            }
          }

        case Failure(t) =>
          context.parent ! Response(cs.imsg, s"An error has occured: " + t.getMessage)
      }

    case imsg: IMessage =>
      CardNameSearch(imsg) match {
        case Some(cs) => self forward cs
        case _ => None
      }
    case _ => None
  }

}
