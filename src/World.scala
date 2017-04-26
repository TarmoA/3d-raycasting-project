
/*
 * Stores the state of the current World as an Array[Wall]
 * A new Array is given with the init method
 * The coordinates of the Walls are relative to the Camera so they change when the camera moves.
 */
object World {
  
  var content = Array[Wall]()
  
  def init(a: Array[Wall]) = {
    content = a
  }
  
  def update(m: Matrix4d) =  {
    content = content.map((r: Wall) => r.multBy(m))
  }
  
}