package v2d2.actions.generic

import akka.actor.{ActorRef, Actor, ActorSystem, ActorContext, Props, ActorLogging}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.pattern.{ask, pipe}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import collection.JavaConversions._
import concurrent.Future
import concurrent.Promise
import java.util.Collection
import org.jivesoftware.smackx.muc.MultiUserChat
import org.jxmpp.util.XmppStringUtils
import scala.collection.immutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import spray.client.pipelining._
import spray.httpx.SprayJsonSupport._
import spray.httpx.encoding.{Gzip, Deflate}
import spray.httpx.unmarshalling.FromResponseUnmarshaller
import v2d2.V2D2
import v2d2.actions.love.LoveJsonProtocol
import v2d2.actions.love.LoveJsonProtocol._
import v2d2.actions.generic.protocol._
import v2d2.client.{IMessage,User}
import v2d2.client.{XHTMLResponse,XHTMLMemo}
import v2d2.actions.generic._
import v2d2.client.core._
import awscala._, ec2._
import com.amazonaws.services.ec2.model.Filter
import com.amazonaws.services.ec2.model.InstanceState
import scala.concurrent.ExecutionContext.Implicits.global

case class Blah(id:String, ip: String, state: InstanceState, name: String)
object Blah {
  def apply(aws: Instance): Blah = {
	Blah(id = aws.instanceId,
         ip = aws.privateIpAddress,
		 name = aws.name,
		 state = aws.state)
  }
}
class ServerAct extends Actor with ActorLogging {
  // implicit val ec2 = EC2.at(Region.US_EAST_1)

  // aws ec2 describe-instances --instance-ids "${id}" |
  // jq '[.Reservations[].Instances[]|
  //     {"name":.Tags|map(select(.Key=="aws:autoscaling:groupName"))[].Value,
  //     "ip":.PrivateIpAddress,
  //     "status":.State.Name, "id":.InstanceId}]'

  // aws ec2 describe-instances --filter "Name=tag-value,Values=${servertag}" |
  // jq '[.Reservations[].Instances[]|
  //     {"name":.Tags|map(select(.Key=="aws:autoscaling:groupName"))[].Value,
  //     "ip":.PrivateIpAddress,
  //     "status":.State.Name, "id":.InstanceId}]'
  def receive: Receive = {
    case imsg: IMessage =>
      Server(imsg) match {
        case Some(server) =>
          log.info("server match")
		  // context.parent ! XHTMLResponse(imsg, new XHTMLMemo())
		  // for { stuff <- Future { ec2.instances(Nil, List(
          //   new Filter().withName("tag-value").withValues("dev-yolo-api"))) }
 		  // } yield(
          //   context.parent ! s"server: ${Blah(stuff.head)}"
		  // )
        case _ => None
      }
  }
}
