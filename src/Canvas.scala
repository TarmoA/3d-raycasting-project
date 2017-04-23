import java.awt.{Graphics2D,Color,Polygon,BasicStroke,Dimension}
import scala.swing._
import scala.swing.event._
import scala.math._

class Canvas extends Panel {
  
  val height = 720
  val width = 720
  preferredSize = new Dimension(width,height)
  focusable = true
  listenTo(keys)
  listenTo(mouse.clicks)
  
  reactions += {
    case KeyPressed(_,key,_,_) => {
      key match {
        case Key.Left   => ModelHandler.startTurning(Left())
        case Key.Right  => ModelHandler.startTurning(Right())
        case Key.A      => ModelHandler.startMoving(Left())
        case Key.D      => ModelHandler.startMoving(Right())
        case Key.W      => ModelHandler.startMoving(Forward())
        case Key.S      => ModelHandler.startMoving(Backward())
        case _ =>
      }
    }
    case KeyReleased(_,key,_,_) => {
      key match {
        case Key.Left   => ModelHandler.stopTurning
        case Key.Right  => ModelHandler.stopTurning
        case Key.A      => ModelHandler.stopMoving(Left())
        case Key.D      => ModelHandler.stopMoving(Right())
        case Key.W      => ModelHandler.stopMoving(Forward())
        case Key.S      => ModelHandler.stopMoving(Backward())
        case _ =>
      }
    }
  }
  
  def update = repaint
 
  override def paintComponent(g: Graphics2D) {
   
    
    g.clearRect(0, 0, size.width, size.height)
    
    var toBeDrawn = {
      World.content.filter(_.toVector.forall((v: Vector4d) =>v.z < Camera.far))
      .filter(_.toVector.exists(_.z > Camera.near))
      .map{w: Wall =>
        if (w.toVector.exists(_.z <= Camera.near))w.projectToZ0
        else w
      }
      .map((r: Wall) =>(r, r.toVector.map(_.z).max))
      .sorted(WallZOrdering).reverse
    }
    
    
    def transformToImgPlane(v: Vector4d) = {
    val x1 = (v.x * width ) / (2.0 * v.w) + width / 2
    val y1 = (v.y * height) / (2.0 * v.w) + height / 2
    Vector4d(x1,y1,v.z,v.w)
    }
    
    val polygons: Array[(Polygon, Double)] = toBeDrawn.map( (a: (Wall, Double)) => 
        Wall(a._1.multBy(Camera.perspectiveTransformer)
        .normalizeHgen.toVector.map(transformToImgPlane(_)) )).map{
      (r: (Wall)) => new Polygon(r.xIntArray, r.yIntArray, 4)
      } zip toBeDrawn.map(_._2)
    
//    if (!toBeDrawn.isEmpty)println(toBeDrawn.map(_.multBy(Matrix4d.perspectiveTransformer).normalize).map(
//        _.toString).reduceLeft(_ + "\n" + _) + "\n")
    

    
    
    
    g.setStroke(new BasicStroke(5f))
    polygons.foreach{p: (Polygon, Double) =>
      g.setColor(Color.black)
      g.drawPolygon(p._1)
      val c = Color.gray
      g.setColor{
        def getC(d: Int) =  {
          val a = 10000/(7500+ p._2)
          max(20,min((a * d).toInt,255))
        }
        new Color(getC(c.getRed), getC(c.getGreen), getC(c.getBlue))
        }
      g.fillPolygon(p._1)
    }
    
    
  }
  
  
}