package v2d2.mtg

import v2d2.actions.generic.HipNotif
import scala.collection.immutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import java.io.InputStream

import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import akka.pattern.{ask, pipe}
import org.jivesoftware.smackx.muc.MultiUserChat
import v2d2.V2D2
import v2d2.client.{IMessage, User}
import v2d2.client.core._
import v2d2.actions.generic.protocol.Response
// import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem}
// import akka.http.scaladsl.Http
// import akka.http.scaladsl.marshalling.Marshal
// import akka.http.scaladsl.model._
// import akka.http.scaladsl.unmarshalling.Unmarshal
// import akka.pattern.ask
// import akka.stream.ActorMaterializer
// import akka.util.Timeout
//
import org.apache.commons.lang3.StringUtils
//
// import v2d2.client.{IMessage, User}
// import v2d2.client.core._

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
        pprint.log(endList.map(t=>t._1 -> t._2.map(c=>c.name)))
        // val lowMap = cards.groupBy(
        //   c => scores(c.name, search)
        //     .groupBy(identity)
        //     .mapValues(_.size)
        //     .toList.sortBy(_._1).head
        // ).toList.sortBy(_._1)
        // pprint.log(lowMap,"orig")
        // pprint.log(lowMap.take(1))
        // pprint.log(lowMap.take(3).map(t=>(t->t._2.map(c=>c.name))))
        // lowMap.head._2 map { c =>
        //   log.info(s"card name: ${c.name} min: ${scores(c.name,search).min} count:${scores(c.name,search).groupBy(identity).mapValues(_.size).toList.sortBy(_._1).head}")
        // } 
        endList.last._2.toList
      case c => 
        // log.info("found someone " + c)
        Tuple2(0, c)._2.toList
    }

    // cards.filter( c =>
    //   c.name.equalsIgnoreCase(search) 
    // ) match {
    //   case Nil =>
    //     log.info("in lookup NIL")
    //     search match {
    //       case _ =>
    //         log.info("in lookup UNAME")
    //         // group by distance sort by distance take the closest list
    //         val out = cards.groupBy( c =>
    //             StringUtils.getLevenshteinDistance(
    //               c.name.toLowerCase(), search)
    //             ).toList.sortBy(_._1).head
    //         log.info(s"in lookup $out")
    //         out._2
    //     }
    //   case c => 
    //     log.info("found someone " + c)
    //     Tuple2(0, c)._2
    // }
      
  }

  def receive: Receive = {
    case mc: MagicCards =>
    // log.info(s"PRESTART XMPP")
    // val stream : InputStream = getClass.getResourceAsStream("/allsets.json")
    // val lines = scala.io.Source.fromInputStream(stream).mkString
    // val req:Future[Map[String,CardSet]] = Unmarshal(lines).to[Map[String,CardSet]]
    //
    // val foo = Unmarshal(lines).to[Map[String,CardSet]]
    // log.info(s"foo: ${foo}")
    // val req = for {
    //   cards <- Unmarshal(lines).to[Map[String,CardSet]]
    // } yield(
    //   cards
    // ) 
    // req.onComplete({
    //   case Success(cs) => {
    //     log.info(s"req complete: ${cs("UNH")}")
    //   }
    //   case Failure(exception) => {
    //     //Do something with my error
    //   }
    // })
    // log.info(s"REQ: ${req}")
      val req = for {
        sets <- Unmarshal(json).to[Map[String,CardSet]]
        } yield(
          // mps_akh, dis
          (sets - "VAN" - "pPRE" - "pMGD" - "CSP" - "8ED" - "PCY"
                - "UNH" - "ONS" - "ODY"
            + ("ON" -> sets("ONS").copy( code = "ON"))
            + ("OD" -> sets("ODY").copy( code = "OD"))
            + ("UH" -> sets("UNH").copy( code = "UH"))
            + ("PR" -> sets("PCY").copy( code = "PR"))
            + ("8EB" -> sets("8ED").copy( code = "8EB"))
            + ("CS" -> sets("CSP").copy( code = "CS"))
          ) map { t =>
            t._1 -> t._2.copy( cards = t._2.cards map { c => 
              c.copy(setKey = Some(t._1)) 
            })
          }
        ) 
      req pipeTo sender
      // req onComplete {
      //   case Success(result:Map[String,CardSet])  =>
      //     log.info("==========================================================")
      //     val test = Map(result.head._1 -> result.head._2.copy(
      //       cards = List(result.head._2.cards.head)))
      //     pprint.log(test, "data")
      //     // result pipeTo sender
      //     // sender ! result
      //     log.info("==========================================================")
      //   case Failure(t) =>
      //     context.parent ! "An error has occured: " + t.getMessage
      // }

    
    case cs:CardNameSearch =>
      val content = for {
        cards <- (self ? MagicCards()).mapTo[Map[String,CardSet]]
      } yield cards
      content onComplete {
        case Success(cards) =>
          val results = lookupName(cs.target.toLowerCase(), cards)
          val uri = "https://magiccards.info/scans/en/"
          val imgs = results collect {
            case c if(c.number != None && c.setKey != None) => c
          } map { c =>
            (uri + c.setKey.get.toLowerCase() + "/" + c.number.get + ".jpg" -> c)
          }

          imgs map { t => println(t._1) }
          if (imgs.length > 4) {
            log.info("send one")
            context.parent ! Response(
              cs.imsg, imgs.map(t=>t._1).mkString("\n"))
          } else {
            log.info("send many")
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
