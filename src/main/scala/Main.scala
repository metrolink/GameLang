//#full-example

//import akka.actor.typed.internal.receptionist.LocalReceptionist.behavior
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, DeathPactException, SupervisorStrategy}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._
import scala.language.postfixOps
//#greeter-actor

object Prisoner{

  sealed trait fightT2 //fight AskToFight and ActorInfo

  final case class AskToFight(replyTo: ActorRef[fightT2], point: Int) extends fightT2

  final case class ActorInfo(name: ActorRef[fightT2], point: Int) extends fightT2

  final case class ChangeTheScore(point: Int) extends fightT2

  sealed trait collision

  final case class CollideWall() extends collision

  final case class CollidePlayer(replyTo: ActorRef[collision], positionX: Int, positionY: Int) extends collision

  final case class CollideGuard(point: Int) extends collision

  sealed trait talk

  final case class conversation(replyTo: ActorRef[talk]) extends talk

  def apply(): Behavior[fightT2] = {
    Behaviors.setup(context => new Prisoner(context).ActorFight())
  }
}

class Prisoner(context: ActorContext[Prisoner.fightT2]) {
  //Behaviors.supervise(behavior).onFailure[DeathPactException](SupervisorStrategy.resume)

  import Prisoner._

  var points = 2000
  var shield = true
  var position = Array.ofDim[Int](2)
  position(0) = 1 //x cordinates
  position(1) = 3 //y cordinates
  implicit val timeout = Timeout(5 seconds)

  /*def ChangeCTF(point: Int): Behavior[fightCtS] = {
    context.self ! ChangeTheScore(point)
    Behaviors.same
  }*/
  def changethescore(): Behavior[fightT2] = {

    //println(context.self.toString + "getting ready to change the score")
    Behaviors.receiveMessage {


      case ChangeTheScore(point) =>
        points += point
        ActorFight()

      case AskToFight(replyTo, point) =>
        Behaviors.unhandled

      case ActorInfo(name, point) =>
        name ! AskToFight(context.self, point)
        changethescore()

    }
  }


  /*def onReply(message: fightP2): Behavior[fightT] = {
  /*case acceptScore: AcceptScore =>
          acceptScore.response match {
            case ChangeTheScore(point) =>
              points += point
              println(context.self.toString + " received " + point)
              onMessage(message)
              Behaviors.same
          }*/
    message match {
      case AskToFight(replyTo, point) =>
        if (shield) {
          points -= point / 2
          println(context.self.toString + "has shield and now has " + points)
          shield = false
        }
        else {
          points -= point * 2
          println(context.self.toString + " lost points and now have: " + points)
        }

        val configAdapter: ActorRef[Prisoner.fightP2] =
          context.messageAdapter { response => AcceptScore(response) }

        configAdapter ! ChangeTheScore(point)
        step2(message)

        if (points < 0) {
          Behaviors.stopped
          context.stop(context.self)
        }
        else {
          Behaviors.same

        }

        replyTo ! AskToFight(context.self, point)

        Behaviors.same
    }
  }*/


  def ActorFight(): Behavior[fightT2] = {
    //(point, name)
    Behaviors.receiveMessagePartial {
      case AskToFight(replyTo, point) =>
        replyTo ! ChangeTheScore(point)
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
          Behaviors.stopped
          context.stop(context.self)
        }


        replyTo ! AskToFight(context.self, point)
        changethescore()


      case ActorInfo(name, point) =>
        name ! AskToFight(context.self, point)
        changethescore()



        def CheckCollition(): Behavior[collision] = Behaviors.receive { (context, message) =>
          message match {
            case CollidePlayer(replyTo, xPos, yPos) =>
              if (xPos == position(0) && yPos == position(1)) {
                replyTo ! CollidePlayer(context.self, position(0), position(1))
                position(0) -= 1
                Behaviors.same
              }
              else
                Behaviors.same

            case CollideWall() =>
              position(0) -= 1
              Behaviors.same

            case CollideGuard(point) =>
              points -= point
              Behaviors.same
          }
          Behaviors.same


          def Conversation(): Behavior[talk] = Behaviors.receive { (context, message) =>
            message match {
              case conversation(replyTo) =>
                println("hello")
                replyTo ! conversation(context.self)
                Behaviors.same
            }
          }

          Behaviors.same
        }

        Behaviors.same
    }
  }
/*
  def whenChangingPoints(message: fightS3): Behavior[Prisoner.fightS3] = {
    message match {
      case AsktoFight(replyTo, point) =>
        if (shield) {
          points -= point / 2
          println(context.self.toString + "has shield and now has " + points)
          shield = false
        }
        else {
          points -= point * 2
          println(context.self.toString + " lost points and now have: " + points)
        }

        val configAdapter: ActorRef[Prisoner.fightS2] =
          context.messageAdapter { response => WrappedFight(response) }

        configAdapter ! ChangeTheScore(point)
        if (points < 0) {
          //replyTo ! StopFighting()
          Behaviors.stopped
          context.stop(context.self)
        }
        else {
          //println(context.self.toString + points)
          Behaviors.same

        }


        //println("fight ended")

        replyTo ! AskToFightAgain(context.self, point)

        Behaviors.same
    }
  }*/
}
//#greeter-actor

//#greeter-bot
/*object Guard {
  var points = 5000
  var shield = false
  var position = Array.ofDim[Int](2)
  position(0) = 1 //x cordinates
  position(1) = 3 //y cordinates



  def apply(): Behavior[Prisoner.fightT] = {
    message match {
      case AskToFight(replyTo, point) =>
        if (shield) {
          points -= point / 2
        }
        else {
          points -= point
        }
        replyTo ! ChangeTheScore(point)
        replyTo ! AskToFight(context.self, point)
      case ActorInfo(name, point) =>
        name ! AskToFight(context.self, point)
      case ChangeTheScore(point) =>
        points += point
        if (points < 0) {
          Behaviors.stopped
        }
        else {
          println(context.self.toString + points)
          Behaviors.same
        }

    }
}*/
//#greeter-bot

//#greeter-main
object Prison {

  final case class StartGame(name: String, points:Int)

  def apply(): Behavior[StartGame] =
    Behaviors.setup { context =>
      //#create-actors
      val prisoner = context.spawn(Prisoner(), "P1")
      val prisoner2 = context.spawn(Prisoner(), "P2")
      val prisoner3 = context.spawn(Prisoner(), "P3")
      //val prisoner4 = context.spawn(Prisoner(), "P4")
      //val prisoner5 = context.spawn(Prisoner(), "P5")
      //#create-actors

      Behaviors.receiveMessage { message =>
        //#create-actors
        //val replyTo = context.spawn(Prisoner(), message.name)
        //#create-actors
        prisoner ! Prisoner.ActorInfo(prisoner2, message.points)
        prisoner ! Prisoner.ActorInfo(prisoner3, message.points)
        //prisoner ! Prisoner.ActorInfo(prisoner3,prisoner4, message.points)
        //prisoner ! Prisoner.ActorInfo(prisoner4, prisoner5, message.points)
        //prisoner ! Prisoner.ActorInfo(prisoner5, prisoner2, message.points)
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
  //prisonMain ! Prison.StartGame("P3", change_points)
  //#main-send-messages
}
//#main-class
//#full-example
