package v2d2.parsers

import fastparse.parsers.Combinators.Rule
import fastparse.parsers._
import scala.reflect.ClassTag
import scala.reflect._
import scala.reflect.runtime.{universe=>ru}
import reflect.runtime.universe.Type
import v2d2.client.IMessage
import fastparse.all._

object MyExtensions {
  import fastparse.all._
  implicit def richStr(str: String) = new {
    val ws:P[Unit] = P(" ".rep)
    val bot:P[Unit] = P(("bot" | "v2d2") ~ ",".?)

    def decode[T] (name:String, cons: (Option[String]) => T): Option[T] = {
      val cmd:P[Unit] = P(name)
      val msg:P[Unit] = P((CharIn('a' to 'z').rep(1) ~ ws.?).rep)
      val fwd:P[(String,String)] = P(bot ~ ws ~ cmd.! ~ ws  ~ "-".? ~ ws ~ msg.?.!)
      val bwk:P[(String,String)] = P(cmd.! ~ ws ~ bot ~ ws  ~ "-".? ~ ws ~ msg.?.!)
      val output:P[T] = P( (fwd|bwk) ~ End).map {
        case (cmd, msg) => cons(Some(msg))
      }
      output.parse(str) match{
        case Parsed.Success(value, _) => Some(value)
        case _ => None
      }
    }
  }
}

class ManualParser[T: ClassTag](name: String, cons: (Option[String]) => T) {
  val ws:P[Unit] = P(" ".rep)
  val bot:P[Unit] = P(("bot" | "v2d2") ~ ",".?)
  val cmd:P[Unit] = P(name)
  val msg:P[Unit] = P((CharIn('a' to 'z').rep(1) ~ ws.?).rep)
  val fwd:P[(String,String)] = P(bot ~ ws ~ cmd.! ~ ws  ~ "-".? ~ ws ~ msg.?.!)
  val bwk:P[(String,String)] = P(cmd.! ~ ws ~ bot ~ ws  ~ "-".? ~ ws ~ msg.?.!)

  def parse(str: String): Option[T] = {
    val output:P[T] = P( (fwd|bwk) ~ End).map {
      case (cmd, msg) => cons(Some(msg))
    }
    output.parse(str) match{
      case Parsed.Success(value, _) => Some(value)
      case _ => None
    }
  }
}

class AutoParser[T: ClassTag] {

  val ws:P[Unit] = P(" ".rep)
  val bot:P[Unit] = P(("bot" | "v2d2" | "!") ~ ",".?)
  val cmd:P[Unit] = P(classTag[T].runtimeClass.getSimpleName.toLowerCase)
  val msg:P[Unit] = P((CharIn('a' to 'z').rep(1) ~ ws.?).rep)
  val fwd:P[(String,String)] = P(bot ~ ws ~ cmd.! ~ ws  ~ "-".? ~ ws ~ msg.?.!)
  val bwk:P[(String,String)] = P(cmd.! ~ ws ~ bot ~ ws  ~ "-".? ~ ws ~ msg.?.!)

  def apply(imsg:IMessage): Option[T] = {
    apply(imsg.content)
  }
  def apply(str: String): Option[T] = {
    val output:P[T] = P( (fwd|bwk) ~ End).map {
      case (cmd, msg) =>
        val mirror = ru.runtimeMirror(getClass.getClassLoader)
        val typeInfo: Type = mirror.classSymbol(classTag[T].runtimeClass).toType
        val cs = typeInfo.typeSymbol.asClass // classSymbol
        val cm = mirror.reflectClass(cs) // class mirror
        val ctor = typeInfo.declaration(ru.nme.CONSTRUCTOR).asMethod // constructor
        val ctorm = cm.reflectConstructor(ctor) // look up constructor in mirror
        ctorm(Some(msg)).asInstanceOf[T]
    }
    output.parse(str) match{
      case Parsed.Success(value, _) => Some(value)
      case _ => None
    }
  }
}

