package v2d2.mtg

import java.io.InputStream

import org.apache.commons.text.similarity._
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
import slack.models.Message
import scala.concurrent.ExecutionContext
import v2d2.protocols.Response

class MagicAct extends Actor with ActorLogging with CardSetProtocol {
  implicit val ec = ExecutionContext.global
  // import system.dispatcher
  implicit val system       = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val timeout      = Timeout(25.seconds)

  val stream: InputStream = getClass.getResourceAsStream("/allsets.json")
  val json                = scala.io.Source.fromInputStream(stream).mkString

  case class Score(
    rawLeven: Int, // score across who string match
    wordMatches: Int, // name: black sun zenith, search: blue sun, value: 1
  )

  def scores2(
    str: String,
    search: String
  ): List[Int] = {
    val symbols = """!<'_-&^'%$#"@=>,""".flatMap(s => s + "|")
    val p       = s"(\\.|\\*|${symbols})"
    str
      .replaceAll(p, "")
      .toLowerCase()
      .split(" ") //["some","card", "name"]
      .map { w =>
        search
          .replaceAll(p, "")
          .split(" ") // ["a", "search", "card"]
          .map { s =>
            StringUtils.getLevenshteinDistance(w, s)
          } // [4, 5, 4], [4, 5, 0] ...
          .sorted 
          .head // 4, 0,  ...
      }
      .toList
      .sorted // [0, 4, 4]
  }



  def scores(
    str: String,
    search: String
  ): List[Int] = {
    val symbols = """!<'_-&^'%$#"@=>,""".flatMap(s => s + "|")
    val p       = s"(\\.|\\*|${symbols})"
    str
      .replaceAll(p, "")
      .toLowerCase()
      .split(" ")
      .map { w =>
        search
          .replaceAll(p, "")
          .split(" ")
          .map { s =>
            StringUtils.getLevenshteinDistance(w, s)
          }
          .sorted
          .head
      }
      .toList
      .sorted
  }

  def lookupName(
    search: String,
    cards: List[ICard]
  ): List[ICard] = {
    cards.filter(c => c.name.equalsIgnoreCase(search)) match {
      case Nil =>
        val minList = cards
          .groupBy(
            c => scores(c.name, search).min
          )
          .toList
          .sortBy(_._1)
          .head
          ._2
        val endList = minList
          .groupBy(
            c =>
              scores(c.name, search)
                .groupBy(identity)
                .mapValues(_.size)
                .toList
                .sortBy(_._1)
                .head
          )
          .toList
          .sortBy(_._1)
        val tList = endList.last._2.groupBy(c => c.name).map(t => t._2.head)
        endList.last._2.groupBy(c => c.name).map(t => t._2.head).toList
      case c =>
        Tuple2(0, c)._2.groupBy(c => c.name).map(t => t._2.head).toList
    }
  }

  def receive: Receive = {
    // case scry:
    case mc: MagicCards =>
      val req = for {
        cards <- Unmarshal(json).to[List[Card]]
      } yield (
        cards
      )
      req.pipeTo(sender)

    case cs: CardNameSearch =>
      val content = for {
        cards <- (self ? MagicCards()).mapTo[List[Card]]
      } yield cards
      content.onComplete {
        case Success(cards) =>
          val target  = cs.target.toLowerCase()
          val results = lookupName(target, cards)
          val score   = scores(results.head.name, cs.target.toLowerCase()).min
          val tlen    = cs.target.length
          val pcent   = (tlen - score).toFloat / tlen

          // println("++++++++++++++++++++++++++++")
          // println(s"pc: ${pcent} score: ${score} len: ${tlen}")
          // println(s"len: ${cs.target.length} target: ${cs.target}")
          // println("++++++++++++++++++++++++++++")

          val jw    = new JaroWinklerDistance()
          val jcent = jw(target, results.head.name.toLowerCase)
          val p     = "%"
          if (cs.target.length < 3) {
            context.parent ! Response(cs.msg, "Try asking again with a longer string")
          } else if (((tlen == 3 || tlen == 4) && score > 1) ||
                     ((tlen == 5 || tlen == 6) && score > 2) || pcent < 0.7) {
            context.parent ! Response(
              cs.msg,
              f""":shrug: your best match was
                 |${results.head.name} with ${pcent * 100}%1.2f$p
                 |and score ${jcent * 100}%1.2f$p""".stripMargin.replaceAll("\n", " ")
            )
          } else {

            val imgs = results.collect {
              case c if (c.image_uris != None) =>
                val u = c.image_uris.get.png.replaceAll("\\?.*$", "")
                // pprint.pprintln(s"uri: ${u}")
                (u -> c)
            }

            // imgs map { t => pprint.pprintln(t._1) }
            if (imgs.length > 16) {
              context.parent ! Response(
                cs.msg,
                f"""Found too many matches (${imgs.length}). The best match was
                   |${results.head.name} with ${pcent * 100}%1.2f$p
                   |and score ${jcent * 100}%1.2f$p""".stripMargin.replaceAll("\n", " ")
              )
            } else {
              context.parent ! Response(
                cs.msg,
                f"""The best match was ${results.head.name} with ${pcent * 100}%1.2f$p
                   |and score ${jcent * 100}%1.2f$p
                   |${imgs.head._1}""".stripMargin.replaceAll("\n", " ")
              )
            }
          }
        case Failure(t) =>
          context.parent ! Response(cs.msg, s"An error has occured: " + t.getMessage)
      }

    case msg: Message =>
      CardNameSearch(msg) match {
        case Some(cs) =>
          self.forward(cs)
        case _ =>
          None
      }
    case _ => None
  }

}
