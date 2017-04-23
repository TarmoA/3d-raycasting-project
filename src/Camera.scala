import scala.math._

object Camera {
  
  private var coords = Vector4d(0,0,0,0)
  
  def location = coords
  
  private val moveSpeed = 5
  private val rotSpeed = scala.math.Pi/180
  
  val viewAngle = Pi/2
  val fov = 1.0/tan(viewAngle/2)
  val near = 1
  val far = 10000
  
  private val t1 = Vector4d(fov,  0,  0,  0)
  private val t2 = Vector4d(0  ,fov,  0,  0)
  private val t3 = Vector4d(0  ,  0  ,(far+near)/(far-near),1)
  private val t4 = Vector4d(0  ,  0  ,(2*near*far)/(near-far)  ,0)
  val perspectiveTransformer = Matrix4d(t1,t2,t3,t4)
  
  def move(d: Set[Direction]) = {
    val collisionDistance = 100
//    val newCoords = coords.add(d.map(_.vector).toVector.reduceLeft(_.add(_)).mult(-moveSpeed))
    val coordDelta = d.map(_.vector).toVector.reduceLeft(_.add(_)).mult(-moveSpeed)
//    World.content.foreach((r: Rect) =>println(r.distanceTo(coordDelta)))
    if (World.content.forall(_.distanceTo(coordDelta) > collisionDistance)) {
    coords = coords.add(coordDelta)
    }
    coords
  }
  // in rads
  private var rotation: Double = 0
  
  def getRotation = rotation
  
  
  /*
   * Rotates the camera and returns the ne rotation in rad
   */
  def rotate(d: Direction) = {
    d match {
      case Right() => rotation += rotSpeed
      case Left() => rotation += -rotSpeed
      case _ =>
    }
    rotation
  }
  
  def resetRotation = rotation = 0
  
}

// y  right left
// z  forwards backwards

abstract class Direction {
  val vector: Vector4d
}

case class Right() extends Direction {
  val vector = Vector4d(1,0,0,0)
}
case class Left() extends Direction {
  val vector = Vector4d(-1,0,0,0)
}
case class Forward() extends Direction {
  val vector = Vector4d(0,0,1,0)
}
case class Backward() extends Direction {
  val vector = Vector4d(0,0,-1,0)
}