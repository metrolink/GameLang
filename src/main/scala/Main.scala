//#full-example

//import akka.actor.typed.internal.receptionist.LocalReceptionist.behavior
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._
import scala.language.postfixOps
//#greeter-actor

object Prisoner{

  sealed trait msgType_T

  final case class msg_AskToFight(replyTo: ActorRef[msgType_T], point: Int) extends msgType_T

  final case class msg_ActorInfo(name: ActorRef[msgType_T], point: Int) extends msgType_T

  final case class msg_ChangeTheScore(point: Int) extends msgType_T

  sealed trait collision

  final case class CollideWall() extends collision

  final case class CollidePlayer(replyTo: ActorRef[collision], positionX: Int, positionY: Int) extends collision

  final case class CollideGuard(point: Int) extends collision

  sealed trait talk

  final case class conversation(replyTo: ActorRef[talk]) extends talk

  def apply(): Behavior[msgType_T] = {
    Behaviors.setup(context => new Prisoner(context).behaviour_B2())
  }
}

class Prisoner(context: ActorContext[Prisoner.msgType_T]) {
  import Prisoner._

  var points = 2000
  var shield = true
  var position = Array.ofDim[Int](2)
  position(0) = 1 //x cordinates
  position(1) = 3 //y cordinates
  implicit val timeout = Timeout(5 seconds)


  def behaviour_B1(): Behavior[msgType_T] = {
    Behaviors.receiveMessagePartial {
      case msg_ChangeTheScore(point) =>
        println("B1: "+ context.self.toString + " RECEIVE msg_ChangeTheScore")
        points += point
        println(context.self.toString + "now has " + points)
        println(context.self.toString + "Behaviour will change from B1 to B2")
        behaviour_B2() //Change behavior

      case msg_AskToFight(replyTo, point) =>
        context.self ! msg_AskToFight(replyTo,point)
        behaviour_B1() //Behavior.same

      case msg_ActorInfo(name, point) =>
        name ! msg_AskToFight(context.self, point)
        behaviour_B1() //Behavior.same
    }


  }

  def behaviour_B2(): Behavior[msgType_T] = {
    //(point, name)
    Behaviors.receiveMessagePartial {
      case msg_AskToFight(replyTo, point) =>
        replyTo ! msg_ChangeTheScore(point)
        if (shield) {
          points -= point / 2
          println(context.self.toString + "has shield and now has " + points)
          shield = false
        }
        else {
          points -= point * 2
          println(context.self.toString + " lost points and now have: " + points)
        }
        if (points < 0) {
          println(context.self.toString + " stopped")
          //Behaviors.stopped
          context.stop(context.self)
        }
        replyTo ! msg_AskToFight(context.self, point)
        behaviour_B1()

      case msg_ChangeTheScore(point) =>
        //Pushes the current message to the back of the mailbox queue.
        context.self ! msg_ChangeTheScore(point)
        behaviour_B2() //Behavior.same

      case msg_ActorInfo(name, point) =>
        name ! msg_AskToFight(context.self, point)
        behaviour_B1() //Change the behavior
    }
  }

  def CheckCollition(): Behavior[collision] = Behaviors.receive { (context, message) =>
          message match {
            case CollidePlayer(replyTo, xPos, yPos) =>
              //if the player position is the same as the opponent, push back
              if (xPos == position(0) && yPos == position(1)) {
                replyTo ! CollidePlayer(context.self, position(0), position(1))
                position(0) -= 1
                Behaviors.same
              }
              else
                Behaviors.same

            case CollideWall() =>
              //Wall is a static object so push back only player
              position(0) -= 1
              Behaviors.same

            case CollideGuard(point) =>
              //if caught by guard, lose points
              points -= point
              Behaviors.same
          }
          Behaviors.same
        }

  def Conversation(): Behavior[talk] = Behaviors.receive { (context, message) =>
            message match {
              case conversation(replyTo) =>
                println("hello")
                replyTo ! conversation(context.self)
                Behaviors.same
            }
          }
        }


object Prison {

  final case class StartGame(name: String, points:Int)

  def apply(): Behavior[StartGame] =
    Behaviors.setup { context =>
      //#create-actors
      val prisoner = context.spawn(Prisoner(), "P1")
      val prisoner2 = context.spawn(Prisoner(), "P2")
      val prisoner3 = context.spawn(Prisoner(), "P3")

      Behaviors.receiveMessage { message =>
        //Send messages to P2 and P3
        prisoner ! Prisoner.msg_ActorInfo(prisoner2, message.points)
        prisoner ! Prisoner.msg_ActorInfo(prisoner3, message.points)
        Behaviors.same
      }
    }
}
//#greeter-main

//#main-class
object AkkaQuickstart extends App {
  //#actor-system
  val customConf = ConfigFactory.parseString(
    """
    akka.log-dead-letters = OFF
    akka.log-dead-letters-during-shutdown = false
  """)
  val prisonMain: ActorSystem[Prison.StartGame] = ActorSystem(Prison(), "AkkaQuickStart", ConfigFactory.load(customConf))
  //#actor-system
  val change_points = 500
  //#main-send-messages
  prisonMain ! Prison.StartGame("P2", change_points)

}
//#main-class
//#full-example
