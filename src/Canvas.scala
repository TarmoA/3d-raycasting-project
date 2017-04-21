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
      World.content.filter(_.toVector.forall((v: Vector4d) => v.z > Camera.near && v.z < Camera.far))
      .sorted(RectZOrdering).reverse
    }
    
    def transformToImgPlane(v: Vector4d) = {
    val x1 = (v.x * width ) / (2.0 * v.w) + width / 2
    val y1 = (v.y * height) / (2.0 * v.w) + height / 2
    Vector4d(x1,y1,v.z,v.w)
    }
    
    
    val polygons = toBeDrawn.map(_.multBy(Camera.perspectiveTransformer)
        .normalize).map((r: Rect) => Rect( r.toVector.map(transformToImgPlane(_)))).map{
      ((a: Rect) => new Polygon(a.xIntArray, a.yIntArray, 4))    
      }
    
//    if (!toBeDrawn.isEmpty)println(toBeDrawn.map(_.multBy(Matrix4d.perspectiveTransformer).normalize).map(
//        _.toString).reduceLeft(_ + "\n" + _) + "\n")
    
//    g.translate(width / 2, height / 2)
    
    g.setStroke(new BasicStroke(5f))
    polygons.foreach{p: Polygon =>
      g.setColor(Color.black)
      g.drawPolygon(p)
      g.setColor(Color.gray)
      g.fillPolygon(p)
    }
    
    
  }
  
 
  
//    case MouseClicked(_,_,_,_,_) => {
//      ModelHandler.changeRotationXZ(Pi/100)
//          println("left")
//          update(ModelHandler.rotationXZ)
//    }
  
}