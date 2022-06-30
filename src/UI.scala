import scala.swing._

class UI extends MainFrame {
  title = "Main"
  val btn1 = Button("Connect4"){
    this.visible = false;
    BoardGamesEngine(Connect4.Drawer,Connect4.Controller)
  }
  val btn2 = Button("Checker"){
    this.visible = false;
    BoardGamesEngine(Checkers.Drawer,Checkers.Controller)
  }
  val btn3 = Button("Chess"){
    this.visible = false;
    BoardGamesEngine(Chess.Drawer,Chess.Controller)
  }
  val btn4 = Button("Tic Tac Toe"){
    this.visible = false;
    BoardGamesEngine(TicTacToe.Drawer,TicTacToe.Controller)
  }
  contents = new BoxPanel(Orientation.Vertical) {
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(5)
      contents += btn1
      contents += btn2
      contents += btn3
      contents += btn4

    }
    contents += Swing.VStrut(5)
    contents += Swing.VStrut(5)
    contents += Swing.VStrut(5)
    contents += Swing.VStrut(5)
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += btn1
      contents += btn2
      contents += btn3
      contents += btn4
      contents += Swing.HGlue
      contents += Button("Close") { reportAndClose() }
    }
    for (e <- contents)
      e.xLayoutAlignment = 0.0
    border = Swing.EmptyBorder(10, 10, 10, 10)
  }

//  listenTo(btn1)
//  listenTo(btn2)
//  listenTo(btn3)
//  listenTo(btn4)

//  reactions += {
//    case ButtonClicked(btn1) => BoardGamesEngine(Connect4.Drawer,Connect4.Controller)
//  }


  def reportAndClose() {
    println("Exit")
    sys.exit(0)
  }
}

object GUI{
  def main(args: Array[String]) {
    val ui = new UI
    ui.visible = true
  }
}