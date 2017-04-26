
/*
 * This is a class for a 4 dimensional vector. First three dimensions are used for regular 3d coordinates and 
 * the fourth one is used for transformation to homogenous coorinates for perspective projection. This is immutable.
 */
class Vector4d(val x: Double, val y: Double, val z: Double, val w: Double) {
  
  /*
   * Size of the vector as a collection
   */
  def size = 4
  
  def apply(i: Int) = vals(i)
  
  /*
   * Length of the vector in 3d space. Does not take into account the w value.
   */
  def length3d = scala.math.sqrt(x*x + y*y + z*z)
  
  private val vals = Vector[Double](x,y,z,w)
  def toVector = vals
  
  /*
  * Adds this together with another vector and returns the new vector.
  */
  def add(other: Vector4d) = {
    new Vector4d(x+other.x, y+other.y, z+other.z, w+other.w)
  }
  
  /*
   * Subtracts another vector from this vector and returns the new vector.
   */
  def sub(other: Vector4d) = {
    new Vector4d(x-other.x, y-other.y, z-other.z, w-other.w)
  }
  
  /*
   * Multiplies this vector with a scalar value(Double) and returns the new vector
   */
  def mult(i: Double) = {
    new Vector4d(x*i, y*i, z*i, w*i)
  }
  
  /*
   * Returns the cross product of this vector and another vector. Only takes into account the coordinates x, y and z
   */
  def crossProd3by3(other: Vector4d) = {
    val newX = this.y*other.z - this.z*other.y
    val newY = this.z*other.x - this.x*other.z
    val newZ = this.x*other.y - this.y*other.x
    Vector4d(newX, newY, newZ, w)
  }
  
  /*
   * Returns the dot product of this vector and another vector. Only takes into account the coordinates x, y and z
   */
  def dotProd3d(v: Vector4d) = {
    x*v.x + y*v.y +  z*v.z
  }
  
  def normalize = Vector4d(x/length3d, y/length3d, z/length3d, w)
  
  /*
   * Normalizes the vector in homogenous coordinates into the w = 1 plane
   */
  def normalizeHgen = Vector4d(x/w, y/w, z/w, 1)
  
  
  override def toString = " (" + x + " ," + y + " ," + z + ", " + w + " )" // for debugging
  
}

object Vector4d {
  
  def apply(x1: Double, y1: Double, z1: Double, w1: Double) = {
    new Vector4d(x1,y1,z1,w1)
  }
  
  def apply(v: Vector[Double]) = {
    new Vector4d(v(0), v(1), v(2), v(3))
  }
  
  
}