import java.awt.{Graphics2D,Color,Polygon,BasicStroke,Dimension}
import scala.swing._
import scala.swing.event._
import scala.math._

class Canvas(val width: Int, val height: Int) extends Panel {
  

  preferredSize = new Dimension(width,height)
  focusable = true
  listenTo(keys)
  
  /*
   * 
   */
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
  
  def update = repaint // this calls paintComponent
 
  override def paintComponent(g: Graphics2D) {
   
    
    g.clearRect(0, 0, size.width, size.height)
    
    /*
     * This takes the World's current content and filters out Walls that are behind the Camera 
     * or too far from the Camera. Pairs the Walls with their maximum z-coordinates and sorts them 
     * by the z-coordinate in descending order.
     * This makes it so that the walls that are closer to the camera(smaller z) are drawn on top of farther away walls
     */
    var toBeDrawn = {
      World.content
      .filter(_.toVector.forall((v: Vector4d) =>v.z < Camera.far)) //Filter away walls that are too far
      .filter(_.toVector.exists(_.z > Camera.near))                //Filter away walls that are behind the camera
      .map{w: Wall =>
        if (w.toVector.exists(_.z <= Camera.near))w.cutAtZ1        //Cuts any walls that are partially behind the camera
        else w                                                     // so that they get rendered correctly
      }
      .map((r: Wall) =>(r, r.toVector.map(_.z).max))
      .sorted(WallZOrdering).reverse
    }
    
    /*
     * Transforms a Vector in homogenous coordinate form to the correct (x,y)-coordinates to be drawn on the screen
     */
    def transformToImgPlane(v: Vector4d) = {
    val x1 = (v.x * width ) / (2.0 * v.w) + width / 2
    val y1 = (v.y * height) / (2.0 * v.w) + height / 2
    Vector4d(x1,y1,v.z,v.w)
    }
    
    /*
     * This transforms the Walls from world-coordinates into correct coordinates on the screen
     * and turns them into java.awt.Polygons to be drawn on the screen
     */
    val polygons: Array[(Polygon, Double)] = toBeDrawn.map( (a: (Wall, Double)) => 
        Wall(a._1.multBy(Camera.perspectiveTransformer)    //Apply a perspective transformation
        .normalizeHgen.toVector                            //Normalize the homogenous coordinates
        .map(transformToImgPlane(_)) ))                    //Transform to coordinates on the screen
        .map{(r: (Wall)) => 
          new Polygon(r.xIntArray, r.yIntArray, 4)
      } zip toBeDrawn.map(_._2)
    
    
    /*
     * This draws the (Polygon, z-coord)-pairs on the screen and applies some shading. Polygons with a bigger z-value
     * are colored darker. Also draws black borders on the Polygons
     */
    g.setStroke(new BasicStroke(5f))
    polygons.foreach{p: (Polygon, Double) =>
      g.setColor(Color.black)
      g.drawPolygon(p._1)
      g.setColor {
    	  val c = Color.gray
        def getC(d: Int) =  {
          val a = 10000/(7500+ p._2)  //some magic numbers I came up with after trial and error for shading
          max(20,min((a * d).toInt,255))
        }
        new Color(getC(c.getRed), getC(c.getGreen), getC(c.getBlue))
      }
      g.fillPolygon(p._1)
    }
    
  }
  
}