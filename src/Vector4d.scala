

class Vector4d(val x: Double, val y: Double, val z: Double, val w: Double) {
  
  def size = 4
  def length3d = scala.math.sqrt(x*x + y*y + z*z)
  
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
  
  def crossProd3by3(other: Vector4d) = {
    val newX = this.y*other.z - this.z*other.y
    val newY = this.z*other.x - this.x*other.z
    val newZ = this.x*other.y - this.y*other.x
    Vector4d(newX, newY, newZ, w)
  }
  
  def dotProd3d(v: Vector4d) = {
    x*v.x + y*v.y +  z*v.z
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
  
  def toVector = vals
  def normalize = Vector4d(x/length3d, y/length3d, z/length3d, w)
  def normalizeHgen = Vector4d(x/w, y/w, z/w, 1)
  
  
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