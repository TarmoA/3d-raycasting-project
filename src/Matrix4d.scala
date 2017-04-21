import scala.math._

class Matrix4d(v1: Vector4d, v2: Vector4d, v3: Vector4d, v4: Vector4d){
  //v1-4 columns
  val columns = Vector(v1,v2,v3,v4)
  
  def mult(v: Vector4d): Vector4d = {
    val newVector = Array[Double](0,0,0,0)
    for (i <- 0 until v1.size){
      var vconstr = 0.0
      for (j <- 0 until columns.size) {
        vconstr =  vconstr + columns(j)(i)*v(j)
      }
      newVector(i) = vconstr
    }
//    println("1: "+v.x+","+v.y+","+v.z)
//    println("2: "+newVector(0)+","+newVector(1)+","+newVector(2))
    Vector4d(newVector(0),newVector(1),newVector(2),newVector(3))
  }
  
  def mult(m: Matrix4d): Matrix4d = {
    val res = m.columns.map(mult(_))
    Matrix4d(res(0), res(1), res(2), res(3))
  }
  
}

object Matrix4d {
//  def apply(v1: Vector[Double], v2: Vector[Double], v3: Vector[Double], v4: Vector[Double]) = {
//    new Matrix4d(v1,v2,v3,v4)
//  }
  
  // HUOM Vector4d:t ovat sarakevektoreita joten matriisit näkyvät tässä muodossa kirjoitettuna "väärin päin"
  
  
  
   def apply(v1: Vector4d, v2: Vector4d, v3: Vector4d, v4: Vector4d) = {
    new Matrix4d(v1,v2,v3,v4)//(v1.toVector,v2.toVector,v3.toVector,v4.toVector)
  }
  
  def getTranslate(v: Vector4d) = {
    val v1 = Vector4d(    1.0,      0,  0,  0)
    val v2 = Vector4d(      0,    1.0,  0,  0)
    val v3 = Vector4d(      0,      0,1.0,  0)
    val v4 = Vector4d(    v.x,    v.y,v.z,1.0)
    
    new Matrix4d(v1,v2,v3,v4)
  }
  
  def getRotateXY(a: Double) {
    val v1 = Vector4d( cos(a), sin(a),  0,  0)
    val v2 = Vector4d(-sin(a), cos(a),  0,  0)
    val v3 = Vector4d(      0,      0,1.0,  0)
    val v4 = Vector4d(      0,      0,  0,1.0)
    
    new Matrix4d(v1,v2,v3,v4)
  }
  
  def getRotateYZ(a: Double) = {
    val v1 = Vector4d(1.0,      0,      0,  0)
    val v2 = Vector4d(  0, cos(a), sin(a),  0)
    val v3 = Vector4d(  0,-sin(a), cos(a),  0)
    val v4 = Vector4d(  0,      0,      0,1.0)
    
    new Matrix4d(v1,v2,v3,v4)
  }
  
  def getRotateXZ(a: Double) = {
    val v1 = Vector4d( cos(a),  0, sin(a),  0)
    val v2 = Vector4d(      0,1.0,      0,  0)
    val v3 = Vector4d(-sin(a),  0, cos(a),  0)
    val v4 = Vector4d(      0,  0,      0,1.0)
    
    new Matrix4d(v1,v2,v3,v4)
  }
  
}