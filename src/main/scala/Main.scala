//#full-example

//import akka.actor.typed.internal.receptionist.LocalReceptionist.behavior

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random
//#greeter-actor

object Prisoner{

  sealed trait msgType_T

  final case class msg_AskToFight(replyTo: ActorRef[msgType_T],xPos:Int, yPos:Int, point: Int) extends msgType_T

  final case class msg_ActorInfo(name: ActorRef[msgType_T], point: Int) extends msgType_T

  final case class msg_ChangeTheScore(point: Int) extends msgType_T

  def apply(): Behavior[msgType_T] = {
    Behaviors.setup(context => new Prisoner(context).behaviour_B2())
  }
}

class Prisoner(context: ActorContext[Prisoner.msgType_T]) {

  import Prisoner._

  val rand = new scala.util.Random
  var score :Int = 2000
  var shield :Boolean = true
  var position = Array.ofDim[Int](2) //(x,y)
  val positionRange :Int = 5
  var xCoordinate :Int = rand.between(1, 10) //x cordinates
  var yCoordinate :Int = rand.between(1, 10) //y cordinates
  //implicit val timeout = Timeout(5 seconds)
  def relocate(): Unit = {
    position(0) = rand.between(1, 10)
    position(1) = rand.between(1, 10)
  }

  def ChangeScoreAndCheckShield(point:Int): Unit = {
    if (shield) {
      score -= point/2
      shield = false
    }
    else {
      score -= point * 2
      println(context.self.toString + " lost points and now have: " + score)
    }
  }

  def CheckIfColliding(xPosition: Int, yPosition: Int): Boolean = {
    if (xPosition <= position(0) + positionRange && xPosition >= position(0) - positionRange //check if players are close
      && yPosition <= position(1) + positionRange && yPosition >= position(1) - positionRange){
      return true
    }
    else{
      return false
    }
  }

  def behaviour_B1(): Behavior[msgType_T] = {
    Behaviors.receiveMessagePartial {
      case msg_ChangeTheScore(point) =>
        score += point
        println(context.self.toString + "now has " + score)
        behaviour_B2 //Change behavior

      case msg_AskToFight(replyTo, xPos, yPos, point) =>
        position(0) = rand.between(1, 10)
        position(1) = rand.between(1, 10)
        //Pushes the current message to the back of the mailbox.
        context.self ! msg_AskToFight(replyTo, xPos, yPos, point)
        behaviour_B1 //Behavior.same

      case msg_ActorInfo(name, point) =>
        name ! msg_AskToFight(context.self, position(0), position(1), point)
        behaviour_B1 //Behavior.same
    }
  }

  def behaviour_B2(): Behavior[msgType_T] = {
    //(point, name)
    Behaviors.receiveMessagePartial {
      case msg_AskToFight(replyTo, xPos, yPos, point) =>
        if (CheckIfColliding(xPos,yPos)) {
          replyTo ! msg_ChangeTheScore(point)

            ChangeScoreAndCheckShield(point)


          if (score < 0) {
            println(context.self.toString + " stopped")
            Behaviors.stopped
          }
          else {
            //Flow.delay(1)
            replyTo ! msg_AskToFight(context.self, position(0), position(1), point)

            behaviour_B1
          }
        } else {
          replyTo ! msg_ChangeTheScore(0)
          relocate()
          behaviour_B2
        }


      case msg_ChangeTheScore(point) =>
        //Pushes the current message to the back of the mailbox queue.
        context.self ! msg_ChangeTheScore(point)
        behaviour_B2 //Behavior.same

      case msg_ActorInfo(name, point) =>
        name ! msg_AskToFight(context.self, position(0), position(1), point)
        behaviour_B1 //Change the behavior
    }
  }
}

object Prison {

  final case class StartGame()
  val change_points = 500
  def apply(): Behavior[StartGame] =
    Behaviors.setup { context =>
      //#create-actors
      val prisoner = context.spawn(Prisoner(), "P1")
      val prisoner2 = context.spawn(Prisoner(), "P2")
      val prisoner3 = context.spawn(Prisoner(), "P3")

      Behaviors.receiveMessage { message =>
        //Send messages to P2 and P3
        prisoner ! Prisoner.msg_ActorInfo(prisoner2, change_points)
        prisoner ! Prisoner.msg_ActorInfo(prisoner3, change_points)
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


  prisonMain ! Prison.StartGame()

}
//#main-class
//#full-example
