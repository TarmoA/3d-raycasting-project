

class Vector4d(val x: Double, val y: Double, val z: Double, val w: Double) {
  
  def size = 4
  
  private val vals = Vector[Double](x,y,z,w)
  
  def add(other: Vector4d) = {
    new Vector4d(x+other.x, y+other.y, z+other.z, w+other.w)
  }
  
  def sub(other: Vector4d) = {
    new Vector4d(x-other.x, y-other.y, z-other.z, w-other.w)
  }
  
  def mult(i: Double) = {
    new Vector4d(x*i, y*i, z*i, w*i)
  }
  
  def apply(i: Int) = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case _ => throw new IndexOutOfBoundsException
    }
  }
  
  def toVector = {
    vals
  }
  
  def normalize = Vector4d(x/w, y/w, z/w, 1)
  
  
  override def toString = " (" + x + " ," + y + " ," + z + ", " + w + " )"
  
}

object Vector4d {
  
  def apply(x1: Double, y1: Double, z1: Double, w1: Double) = {
    new Vector4d(x1,y1,z1,w1)
  }
  
  def apply(v: Vector[Double]) = {
    new Vector4d(v(0), v(1), v(2), v(3))
  }
  
  
}