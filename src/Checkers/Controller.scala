package Checkers

import Model.{Controller, State}

object Controller extends Controller{
  override def apply(state:State,input:String): State = {
    if(state==null) {//initialization
      var temp:State = null
      temp = new State
      temp.board = Array.ofDim[Char](8, 8)
      temp.board= Array(
        Array('b', ' ', 'b', ' ', 'b', ' ', 'b', ' '),
        Array(' ', 'b', ' ', 'b', ' ', 'b', ' ', 'b'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array('w', ' ', 'w', ' ', 'w', ' ', 'w', ' '),
        Array(' ', 'w', ' ', 'w', ' ', 'w', ' ', 'w'))
      return temp
    }
    val inputToIndex: String => Array[Int] = { input => val index: Array[Int] = Array.fill[Int](4)(-1)
      try {
        index(0) = "abcdefgh".indexOf(input.charAt(0)) // X1
        index(1) = "87654321".indexOf(input.charAt(1)) // Y1
        index(2) = "abcdefgh".indexOf(input.charAt(2)) // X2
        index(3) = "87654321".indexOf(input.charAt(3)) // Y2
      }
      catch {
        case x: StringIndexOutOfBoundsException =>
      }
      index
    }
    

    val index: Array[Int] = inputToIndex(input)

    val calculateDiagonalDistance: Array[Int] => Int = { index =>
      val deltaX = index(0) - index(2)
      val deltaY = index(1) - index(3)
      if (index.contains(-1))
        0 // 0 Units move
      else if (math.abs(deltaX) == 1 && math.abs(deltaY) == 1)
        1 // 1 Unit move
      else if (math.abs(deltaX) == 2 && math.abs(deltaY) == 2)
        2 // 2 Units move
      else
        0 // 0 Units move
    }

    val valid: Int = calculateDiagonalDistance(index)

    if (valid == 0) {
      return state
    }
    if (state.board(index(3))(index(2)) == ' ') {
      // black
      if (state.board(index(1))(index(0)) == 'b' && !state.turn && index(3) - index(1) == valid) {
        if (valid == 2) {
          val temp = state.board(index(3) - 1)((index(2) + index(0)) / 2)
          if (temp == 'w' || temp == 'W')
            state.board(index(3) - 1)((index(2) + index(0)) / 2) = ' '
          else {
            return state
          }
        }
        state.board(index(1))(index(0)) = ' '
        if (index(3) == 7)
          state.board(index(3))(index(2)) = 'B'
        else
          state.board(index(3))(index(2)) = 'b'

      }
      // move spacial black
      else if (state.board(index(1))(index(0)) == 'B' || state.board(index(1))(index(0)) == 'W') {
        if (valid == 1) {
          if (state.turn && state.board(index(1))(index(0)) == 'W')
            state.board(index(3))(index(2)) = 'W'
          else if (!state.turn && state.board(index(1))(index(0)) == 'B')
            state.board(index(3))(index(2)) = 'B'
          else {
            return state
          }
          state.board(index(1))(index(0)) = ' '
        }
        else {
          val temp = state.board((index(1) + index(3)) / 2)((index(0) + index(2)) / 2)
          if (state.turn && state.board(index(1))(index(0)) == 'W') {
            if (temp == 'b' || temp == 'B') {
              state.board((index(1) + index(3)) / 2)((index(0) + index(2)) / 2) = ' '
              state.board(index(1))(index(0)) = ' '
              state.board(index(3))(index(2)) = 'W'
            }
            else {
              return state
            }
          }
          else if (!state.turn && state.board(index(1))(index(0)) == 'B') {
            if (temp == 'w' || temp == 'W') {
              state.board((index(1) + index(3)) / 2)((index(0) + index(2)) / 2) = ' '
              state.board(index(1))(index(0)) = ' '
              state.board(index(3))(index(2)) = 'B'
            }
            else {
              return state
            }
          }
        }
      }

      // move w
      else if (state.board(index(1))(index(0)) == 'w' && state.turn && index(1) - index(3) == valid) {
        if (valid == 2) {
          if (state.board(index(3) + 1)((index(2) + index(0)) / 2) == 'b' || state.board(index(3) + 1)((index(2) + index(0)) / 2) == 'B')
            state.board(index(3) + 1)((index(2) + index(0)) / 2) = ' '
          else {
            return state
          }
        }
        state.board(index(1))(index(0)) = ' '
        if (index(3) == 0)
          state.board(index(3))(index(2)) = 'W'
        else state.board(index(3))(index(2)) = 'w'

      }
      else {
        return state
      }
    }
    else {
      return state
    }

    for (i <- 0 to 7) {
      print("87654321".charAt(i) + "\t")
      for (j <- 0 to 7) {
        print(state.board(i)(j) + " ")
      }
      println()
    }
    println("\ta b c d e f g h")

    state.turn= !state.turn
    return state
  }
}
