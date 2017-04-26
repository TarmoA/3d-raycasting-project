import scala.math._


/*
 * This is the camera that a user controls
 */
object Camera {
  
  /*
   * Cameras position relative to the starting position
   */
  private var coords = Vector4d(0,0,0,0)
  def location = coords
  
  private val moveSpeed = 5
  private val rotSpeed = scala.math.Pi/180
  
  /*
   * These can be tweaked to achieve viewing angles other than 90 degrees
   */
  val viewAngle = Pi/2
  val fov = 1.0/tan(viewAngle/2)
  val near = 1      // Walls with a z-value smaller than this are not rendered
  val far = 10000   // Walls with a z-value greater than this are not rendered
  
  /*
   * This is the matrix to apply a perspective transform on a point
   */
  private val t1 = Vector4d(fov,  0,  0,  0)
  private val t2 = Vector4d(0  ,fov,  0,  0)
  private val t3 = Vector4d(0  ,  0  ,(far+near)/(far-near),1)
  private val t4 = Vector4d(0  ,  0  ,(2*near*far)/(near-far)  ,0)
  val perspectiveTransformer = Matrix4d(t1,t2,t3,t4)
  
  /*
   * Moves the Camera by adding the normalized sum of direction-vectors to the camera's current location. If there would
   * be a collision with a wall in that direction, does not move.
   */
  def move(d: Set[Direction]) = {
    val collisionDistance = 100
    val coordDelta = d.map(_.vector).toVector.reduceLeft(_.add(_)).normalize.mult(-moveSpeed)
    if (World.content.forall(_.distanceTo(coordDelta) > collisionDistance)) {
    coords = coords.add(coordDelta)
    }
    coords
  }
  
  private var rotation: Double = 0  // in rads
  def getRotation = rotation
  
  
  /*
   * Rotates the camera and returns the new rotation in radians
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

/*
 * Class representing a direction of movement. Each direction has a unit vector that represents the direction.
 */
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