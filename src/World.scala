

object World {
  
  var content = scala.collection.mutable.ArrayBuffer[Rect]()
  
  
  def createRect(x1: Double, z1: Double, x2: Double, z2: Double) = {
    val v1 = Vector4d(  x1,-300, z1, 1)
    val v2 = Vector4d(  x1, 300, z1, 1)
    val v3 = Vector4d(  x2, 300, z2, 1)
    val v4 = Vector4d(  x2,-300, z2, 1)
    new Rect(v1,v2,v3,v4)
  }
  
  val wall1 = createRect(300,800,-300,800)
  val wall2 = createRect(300,200,-300,200)
  val wall3 = createRect(300,800, 300,200)
  content = content ++ Array(wall1, wall2, wall3)
  
  var needsUpdating = false
  
  def update(m: Matrix4d) =  {
    content = content.map((r: Rect) => r.multBy(m))
//    println(content.map(_.toString).reduceLeft(_+ "\n" + _ ) + "\n")
  }
  
}