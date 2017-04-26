import scala.swing._
import scala.swing.event._
import java.awt.{Rectangle,Color,Polygon,Graphics,BasicStroke}
import scala.math._


object Main extends SimpleSwingApplication {
  
  val canvas = new Canvas(720, 720)
  
  val menu = new Menu
    
  val top = new MainFrame {
    contents = menu
    
    /*
     * This updates the state of the world and redraws on the canvas every time the timer ticks
     */
    val logicUpdater = new java.awt.event.ActionListener{
      def actionPerformed(e: java.awt.event.ActionEvent) = {
        ModelHandler.update
        canvas.update
      }
    }
    
    val timer = new javax.swing.Timer(6,logicUpdater)
    
    /*
     * This tries to load a world from a given file path. If succesful, initializes the world , puts the Canvas
     * on the screen and starts the timer.
     * If it fails, lets the Menu know of an error
     */
    def load(path: String) = {
      val worldContent = WorldLoader.load(path)
      if (worldContent.isDefined) {
    	  timer.start
    	  World.init(worldContent.get)
        contents = canvas
        canvas.requestFocus
      } else {
        menu.onError
      }
    }
    
  }
  
}

