package TicTacToe
import Model._

object Controller extends Controller {
  override def apply(state:State,input:String): State = {
    //initialization
    if(state == null) {
      var temp:State = null
      temp = new State
      temp.board = Array.ofDim[Char](3,3)
      temp.board = Array(
        Array(' ', ' ', ' '),
        Array(' ', ' ', ' '),
        Array(' ', ' ', ' ')
      )
      return temp
    }
    val row:Int = (input.charAt(0) - '1').toInt
    val col:Int = (input.charAt(1) - '1').toInt
    print(row+"     "+col)
    //turn = true -> player1 turn (X)
    //turn = false -> player2 turn (O)

    def valid(ip:String): Boolean = {
      if(row < 0 || row >= 3 || col < 0 || col >= 3 || input.length != 2)
        return false
      if(state.board(row)(col) != ' ')
        return false
      true
    }

    if (valid(input)) {

      if(state.turn)
        state.board(row)(col) = 'X'
      else
        state.board(row)(col) = 'O'

      state.turn = !state.turn //change turn

      //for testing will be removed later
      for(i<- 0 to 2){
        for(j<- 0 to 2){
          print(state.board(i)(j) + " ")
        }
        println()
      }

    }
    state
  }
}