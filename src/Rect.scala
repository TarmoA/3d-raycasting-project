

class Rect(v1: Vector4d, v2: Vector4d, v3: Vector4d, v4: Vector4d) {
  
  private val vectors = Vector[Vector4d](v1,v2,v3,v4)
  
  def center = v1.add(v3.mult(0.5))
  
  def xIntArray = vectors.map(_.x.toInt).toArray
  
  def yIntArray = vectors.map(_.y.toInt).toArray
  
  def multBy(m: Matrix4d) = Rect(vectors.map(m.mult(_))) 
  
  def translate(v: Vector4d) = Rect(vectors.map(_.add(v)))
  
  
  
  def normalize = Rect(vectors.map(_.normalize))
  
  def toVector = vectors
  
  override def toString = {
    v1.toString + "\n" + v2.toString + "\n" +
    v3.toString + "\n" + v4.toString + "\n" 
  }
}

object Rect {
  
  def apply(v1: Vector4d, v2: Vector4d, v3: Vector4d, v4: Vector4d): Rect =  {
    new Rect(v1: Vector4d, v2: Vector4d, v3: Vector4d, v4: Vector4d) 
  }
  
  def apply(v: Vector[Vector4d]): Rect = {
    new Rect(v(0), v(1), v(2), v(3))
  }
  
}

object RectZOrdering extends Ordering[Rect] {
  def compare(a: Rect, b: Rect) = {
    a.toVector.map(_.z).max compare b.toVector.map(_.z).max
  }
}