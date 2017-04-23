import scala.io.Source

object WorldLoader {
  
  def load(file: String): Array[Wall] = {
    
    def createWall(x1: Double, z1: Double, x2: Double, z2: Double) = {
      val v1 = Vector4d(  x1,-300, z1, 1)
      val v2 = Vector4d(  x1, 300, z1, 1)
      val v3 = Vector4d(  x2, 300, z2, 1)
      val v4 = Vector4d(  x2,-300, z2, 1)
      new Wall(v1,v2,v3,v4)
    }
    val walls = scala.collection.mutable.ArrayBuffer[Wall]()
    try {
      val data = Source.fromFile(file)
      val lines = data.getLines
      
      /*
       * format for data:
       * x1,z1 ; x2,z2
       */
      
      
      while (lines.hasNext) {
        val l = lines.next.split(";").map(_.split(",").map(_.trim.toDouble))
        walls += createWall(l(0)(0), l(0)(1), l(1)(0), l(1)(1))
      }
    } catch {
      case _: java.io.IOException =>("Something went wrong. Possibly wrong filename/path")
      case _: Throwable => println("Something went wrong. Possibly wrong file format")
    }

    walls.toArray
  }
  
}