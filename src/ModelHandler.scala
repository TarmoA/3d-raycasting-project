/*
 * This is the object responsible for updating the internal logic of the world.
 */
object ModelHandler {
  
  /*
   * Contains the current direction that the camera should be turning to, if any
   */
  private var turning: Option[Direction] = None
  
  /*
   * Contains the current set of directions that the camera shoud be moving towards, if any
   */
  private var moving = Set[Direction]()
  
  
  /*
   * These are triggered by key-events coming from the Canvas
   */
  def startMoving (d: Direction) = moving = moving.+(d)
  def stopMoving(d: Direction) = moving = moving.-(d)
  
  def startTurning (d: Direction) = turning =  Some(d) 
  def stopTurning = turning = None
  
  
  var oldPos = Camera.location
  var oldRot = Camera.getRotation
  
  /*
   * Creates a new matrix that is used to multiply all the points of the current World to get the correct rotations 
   * and translations(movement of the camera)
   */
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
      matrices = matrices.+:(Matrix4d.getRotateY(rotDelta))
      oldRot = newRot
    }
    
    
    if (!matrices.isEmpty) {
      val updateMatrix = matrices.reduceLeft(_.mult(_))
      World.update(updateMatrix)
    }
    
  }
  
}