import scala.swing._
import scala.swing.event._

/*
 * This class is used to give the user a text field to define a file from which to load a world
 */
class Menu extends BoxPanel(Orientation.Vertical) {
  
  focusable = true
  listenTo(keys)
  
  val field = new TextField
  val label = new Label("Please enter a filename to load")
  val errorLabel = new Label(" ")
  val button = new Button("Load")
  listenTo(button)
  
  contents += label
  contents += field
  contents += button
  contents += errorLabel
 
  reactions += {
    case ButtonClicked(src) => {
  	  src match {
  	  case button => load
  	  }
    }
  }
  
  def load = Main.top.load(field.text)
  
  def onError = errorLabel.text = "error trying to load"
}