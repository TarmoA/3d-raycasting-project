import scala.swing._
import scala.swing.event._
import java.awt.{Rectangle,Color,Polygon,Graphics,BasicStroke}
import scala.math._


object Main extends SimpleSwingApplication {
  
  val canvas = new Canvas
  
    
  def top = new MainFrame {
    title = "asd"
    contents = canvas
  }
  
  val logicUpdater = new java.awt.event.ActionListener{
    def actionPerformed(e: java.awt.event.ActionEvent) = {
    ModelHandler.update
    canvas.update
    }
   
    
  }
  
  val timer = new javax.swing.Timer(6,logicUpdater)
  timer.start
  
}

object ModelHandler {
  
  private var turning: Option[Direction] = None
  private var moving = Set[Direction]()
  
  def startMoving (d: Direction) = moving = moving.+(d)
  def stopMoving(d: Direction) = moving = moving.-(d)
  def startTurning (d: Direction) = turning =  Some(d) 
  def stopTurning = turning = None
  
  
  var rotationXZ: Double = 0
  
  var oldPos = Camera.location
  var oldRot = Camera.getRotation
  def update = {
    var matrices = Array[Matrix4d]()
    
    if (!moving.isEmpty) {
      val newPos = Camera.move(moving)
      val posDelta = Camera.location.sub(oldPos)
      matrices = matrices.+:(Matrix4d.getTranslate(posDelta))
      oldPos = newPos
    }
    
    if (turning.isDefined) {
      val newRot = Camera.rotate(turning.get)
      val rotDelta = Camera.getRotation - oldRot
      matrices = matrices.+:(Matrix4d.getRotateXZ(rotDelta))
      oldRot = newRot
    }
    
    
    if (!matrices.isEmpty) {
      val updateMatrix = matrices.reduceLeft(_.mult(_))
      World.update(updateMatrix)
    }
    
//    println(updateMatrix.columns.map(_.toString).reduceLeft(_ + "\n" +_) + "\n")
  }
  

  
  
  
}