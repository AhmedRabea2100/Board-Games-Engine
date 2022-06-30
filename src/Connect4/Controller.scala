package Connect4
import Model._
object Controller extends Controller {
  override def apply(state:State,input:String): State = {
    if(state == null) {//initialization
      var temp:State = null
      temp = new State
      temp.board = Array.ofDim[Char](6,7)
      temp.board = Array(
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ')
      )
      return temp
    }

    val col:Int = (input.charAt(0) - '1').toInt

    //turn = true -> player1 turn (R) red color
    //turn = false -> player2 turn (Y) yellow color
    var place:Int = 6;

    def valid(ip:String): Boolean = {
      var i:Int = 0
      if(col < 0 || col >= 7 || input.length != 1)
        return false
      while(i < 6){
        if(state.board(i)(col) == ' ')
          place = i
        i = i + 1
      }
      if(place == 6)
        return false
      true
    }

    if (valid(input)) {
      print("valid")
      if(state.turn)
        state.board(place)(col) = 'R'
      else
        state.board(place)(col) = 'Y'
      state.turn = !state.turn //change turn

      //for testing will be removed later
//      for(i<- 0 to 5){
//        for(j<- 0 to 6){
//          print(state.board(i)(j) + " ")
//        }
//        println()
//      }

    }
    state
  }
}
