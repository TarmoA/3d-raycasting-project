

object World {
  
  var content = WorldLoader.load("test.lvl")
  
  def update(m: Matrix4d) =  {
    content = content.map((r: Wall) => r.multBy(m))
//    println(content.map(_.toString).reduceLeft(_+ "\n" + _ ) + "\n")
  }
  
}