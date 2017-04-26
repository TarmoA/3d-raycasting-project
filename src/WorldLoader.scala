import scala.io.Source

object WorldLoader {
  
  /*
   * Tries to load a world from file and returns it wrapped in an option. Returns None if there is any exception
   * when loading the world.
   */
  
  def load(file: String): Option[Array[Wall]] = {
    
    /*
     * Helper function to create a wall with a fixed height(y) from a pair of (x,z) coordinates
     * Creates a wall in the following form:
     *  
     * v2__v3
     *  |__|
     * v1	 v4
     */
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
      case _: java.io.IOException => return None//("Something went wrong. Possibly wrong filename/path")
      case _: Throwable => return None //println("Something went wrong. Possibly wrong file format")
    }

    Some(walls.toArray)
  }
  
}