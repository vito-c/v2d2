// package v2d2.client.core
//
// import v2d2.client.core.{ActorMeta,FindUser}
// import v2d2.client.User
// import scala.collection.immutable
// import scala.concurrent.Future
// import scala.concurrent.duration._
// import scala.util.{Failure, Success}
// import org.joda.time.DateTime
// import java.net.URL
//
// import akka.actor.{Actor, ActorContext, ActorLogging, ActorSystem}
// import akka.http.scaladsl.Http
// import akka.http.scaladsl.model._
// import akka.http.scaladsl.model.headers.RawHeader
// import akka.http.scaladsl.unmarshalling.Unmarshal
// import akka.pattern.{ask, pipe}
// import akka.stream.ActorMaterializer
// import akka.util.Timeout
// import v2d2.V2D2
// import v2d2.actions.generic.protocol.Response
// import v2d2.client.IMessage
//
// trait ActorMeta {
//   def xmpp = { context.actorSelection("/user/xmpp") }
//
// }
