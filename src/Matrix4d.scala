import scala.math._

/*
 * A 4-by-4 matrix that is composed of 4 Vector4d-objects. The vectors are its column-vectors.
 */
class Matrix4d(v1: Vector4d, v2: Vector4d, v3: Vector4d, v4: Vector4d){
  
  val columns = Vector(v1,v2,v3,v4)
  
  /*
   * Matrix-vector multiplication
   */
  def mult(v: Vector4d): Vector4d = {
    val newVector = Array[Double](0,0,0,0)
    for (i <- 0 until v1.size){
      var vconstr = 0.0
      for (j <- 0 until columns.size) {
        vconstr =  vconstr + columns(j)(i)*v(j)
      }
      newVector(i) = vconstr
    }
    Vector4d(newVector(0),newVector(1),newVector(2),newVector(3))
  }
  
  /*
   * Matrix-matrix multiplication
   */
  def mult(m: Matrix4d): Matrix4d = {
    val res = m.columns.map(mult(_))
    Matrix4d(res(0), res(1), res(2), res(3))
  }
  
}

object Matrix4d {
  // The Vector4ds are column vectors so they look like they are the wrong way on the below matrices
 
   def apply(v1: Vector4d, v2: Vector4d, v3: Vector4d, v4: Vector4d) = {
    new Matrix4d(v1,v2,v3,v4)
  }
  
   /*
    * Matrix used to translate a vector
    */
  def getTranslate(v: Vector4d) = {
    val v1 = Vector4d(    1.0,      0,  0,  0)
    val v2 = Vector4d(      0,    1.0,  0,  0)
    val v3 = Vector4d(      0,      0,1.0,  0)
    val v4 = Vector4d(    v.x,    v.y,v.z,1.0)
    
    new Matrix4d(v1,v2,v3,v4)
  }
  
  /*
   * Matrices that rotate a point around an axis(x, y or z) by an amount given in radians
   */
  def getRotateZ(a: Double) {
    val v1 = Vector4d( cos(a), sin(a),  0,  0)
    val v2 = Vector4d(-sin(a), cos(a),  0,  0)
    val v3 = Vector4d(      0,      0,1.0,  0)
    val v4 = Vector4d(      0,      0,  0,1.0)
    new Matrix4d(v1,v2,v3,v4)
  }
  
  def getRotateX(a: Double) = {
    val v1 = Vector4d(1.0,      0,      0,  0)
    val v2 = Vector4d(  0, cos(a), sin(a),  0)
    val v3 = Vector4d(  0,-sin(a), cos(a),  0)
    val v4 = Vector4d(  0,      0,      0,1.0)
    new Matrix4d(v1,v2,v3,v4)
  }
  
  def getRotateY(a: Double) = {
    val v1 = Vector4d( cos(a),  0, sin(a),  0)
    val v2 = Vector4d(      0,1.0,      0,  0)
    val v3 = Vector4d(-sin(a),  0, cos(a),  0)
    val v4 = Vector4d(      0,  0,      0,1.0)
    new Matrix4d(v1,v2,v3,v4)
  }
  
}