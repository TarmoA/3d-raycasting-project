import scala.math._

/*
 * This is a class representing a wall. Composed of four Vector4d-objects that represent the four corners of the wall. 
 * This is immutable
 */
class Wall(v1: Vector4d, v2: Vector4d, v3: Vector4d, v4: Vector4d) {
  
  private val vectors = Vector[Vector4d](v1,v2,v3,v4)
  
  /*
   * These methods are used to return the walls coordinates as an Array of x- or y- values for the drawPolygon-method
   * of java.awt
   */
  def xIntArray = vectors.map(_.x.toInt).toArray
  def yIntArray = vectors.map(_.y.toInt).toArray
  
  def multBy(m: Matrix4d) = Wall(vectors.map(m.mult(_))) 
  
  def translate(v: Vector4d) = Wall(vectors.map(_.add(v)))
  
  /*
   * This returns the shortest distance from the wall to a Vector4d point. Only works if the wall stands straight up
   * or if v1 and v2 have the same (x,z) coordinates and v3 and v4 have the same (x,z) coordinates
   */
  def distanceTo(v: Vector4d): Double = {
    val a1 = v1.add(v)
    val a3 = v3.add(v)
   
    val l1 = Vector4d(a1.x,0,a1.z,1)
    val l3 = Vector4d(a3.x,0,a3.z,1)
    val d = abs(a3.x*a1.z - a3.z*a1.x) / sqrt(pow(a3.z - a1.z, 2) + pow(a3.x - a1.x, 2) )
    val lnLength = l1.sub(l3).length3d
    
    if (l1.length3d > lnLength + d) return l3.length3d
    if (l3.length3d > lnLength + d) return l1.length3d
    d
  }
  /*
   * This method cuts the Wall so that any part of it that has a z-coordinate smaller than 1 is vut off and the new end
   * of the wall is set to z = 1.
   * This is used to correctly render Walls that would otherwise be partially behind the camera
   */
  def cutAtZ1 = {
    def getNew(v: Vector4d, a: Vector4d) = {
      val lineV = a.sub(v)
      val zV = Vector4d(0,0,-v.z + 1,1)
      val newLength = zV.length3d / (lineV.dotProd3d(zV)/(lineV.length3d*zV.length3d))
      val n = v.add(lineV.normalize.mult(newLength))
      Vector4d(n.x, n.y, 1, 1)
    }
    if (v1.z < 0) {
      val n1 = getNew(v1,v4)
      val n2 = getNew(v2,v3)
      Wall(n1,n2, v3, v4)
    } else {
      val n4 = getNew(v4,v1)
      val n3 = getNew(v3,v2)
      Wall(v1,v2, n3, n4)
    }
  }
  
  
  def normalizeHgen = Wall(vectors.map(_.normalizeHgen))
  
  def toVector = vectors
  
  override def toString = {
    v1.toString + "\n" + v2.toString + "\n" +
    v3.toString + "\n" + v4.toString + "\n" 
  }
  
  /*
   * Gets the normalized(length = 1) normal of this Wall
   */
  def normal = {
    val n = v2.sub(v1).crossProd3by3(v4.sub(v1))
    n.normalize
    
  }
}

object Wall {
  def apply(v1: Vector4d, v2: Vector4d, v3: Vector4d, v4: Vector4d): Wall =  {
    new Wall(v1: Vector4d, v2: Vector4d, v3: Vector4d, v4: Vector4d) 
  }
  
  def apply(v: Vector[Vector4d]): Wall = {
    new Wall(v(0), v(1), v(2), v(3))
  }  
}

/*
 * Used to order a Pair (Wall, max z-coordinate of wall)
 * by the z coordinate.
 * If two pairs have the same z-coordinate they are also ordered on their minimum z.coordinates.
 */
object WallZOrdering extends Ordering[(Wall, Double)] {
  def compare(a: (Wall, Double), b: (Wall, Double)) = {
    val aMax = a._2
    val bMax = b._2
     if (aMax != bMax) aMax compare bMax
     else a._1.toVector.map(_.z).min compare b._1.toVector.map(_.z).min
  }
}