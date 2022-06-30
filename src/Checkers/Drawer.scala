package Checkers

import Model._

import java.awt.{Color, Graphics}
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import scala.swing.event.{Key, KeyPressed, KeyReleased}
import scala.swing.{AbstractButton, Action, BoxPanel, Button, Dimension, Frame, Label, MainFrame, Orientation, Swing, TextField, ToggleButton}

object Drawer extends Drawer {
  override def apply(state: State, controller: Controller): Unit = {
    val ui = new UI(state, controller)
    ui.visible = true
  }
}

class UI(state: State, controller: Controller) extends MainFrame {
  title = "Checkers"
  val nameField = new TextField {
    columns = 32
  }
  var frame = new Board()
  frame.reDraw
  contents = new BoxPanel(Orientation.Vertical) {
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new Label("Input")
      contents += Swing.HStrut(5)
      contents += nameField
      contents += Button("Make The Move") {
        click()
      }
    }

    contents += frame
    contents += Swing.VStrut(5)
    contents += Swing.VStrut(5)
    contents += Swing.VStrut(5)
    contents += Swing.VStrut(5)
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents+= new Label("               a                     b                     c                     d                     e                     f                     g                     h")
    }
    for (e <- contents)
      e.xLayoutAlignment = 0.0
    border = Swing.EmptyBorder(10, 10, 10, 10)
  }

  //  listenTo(pressMe)
  listenTo(nameField.keys)
  reactions += {
    case KeyPressed(nameField, Key.Enter, _, _) => {
      click()
    }
  }

  def click() = {
    frame.reDraw
    //    this.dispose()
    this.visible = true
    this.repaint();
    this.nameField.text = ""
  }

  class Board extends BoxPanel(Orientation.Vertical) {
    def reDraw: Unit = {
      var button: Btn = null;
      val siz = new Dimension(75, 75)
      if (contents.size > 0)
        contents.remove(0, 8)
      var s: State = controller.apply(state, if (nameField.text == "") "a1 b1" else nameField.text)
      for (i <- 0 to 7) {
        contents += {
          new BoxPanel(Orientation.Horizontal) {
            contents+=new Label(8-i+"  ");
            for (j <- 0 to 7) {

              button = new Btn(siz, s.board(i)(j) + "")

              if((i+j)%2==0) button.background=Color.DARK_GRAY
              if (s.board(i)(j) == 'w') {
                button.icon = new javax.swing.ImageIcon(getClass().getResource("w.png"))
              }
              else if (s.board(i)(j) == 'b') {
                button.icon = new javax.swing.ImageIcon(getClass().getResource("b.png"))
              }
              else if (s.board(i)(j) == 'B') {
                button.icon = new javax.swing.ImageIcon(getClass().getResource("bs.png"))
              }
              else if (s.board(i)(j) == 'W') {
                button.icon = new javax.swing.ImageIcon(getClass().getResource("ws.png"))
              }

              contents += button
            }
            var temp = new Btn(new Dimension(20,20),"");
            temp.setRadius(20)
            if(state.turn)
              temp.background = Color.WHITE
            else
              temp.background = Color.BLACK
            contents+=temp
          }
        }
      }
    }
  }


}

