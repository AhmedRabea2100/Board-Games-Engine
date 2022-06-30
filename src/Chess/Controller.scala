package Chess

import Model._

import scala.annotation.tailrec

object Controller extends Controller {
  override def apply(state:State,input:String): State = {
    if(state==null) {//initialization
      var temp:State = null
      temp = new State
      temp.board = Array.ofDim[Char](6,7)
      temp.board = Array(
        Array('R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R'),
        Array('P', 'P', 'P', 'P', 'P', 'P', 'P', 'P'),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
        Array('p', 'p', 'p', 'p', 'p', 'p', 'p', 'p'),
        Array('r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'),
      )
//      initBoard(state.board)
      return temp
    }
    if (correctMove(input, state)) {//validation of move
      state.turn = !state.turn//change turn
    }
    state

  }

  /**
   * Function to set up initial state of board.
   *
   * @param board 2D array of characters used to store board state.
   */
  private def initBoard(board: Array[Array[Char]]): Unit = {
    board map (_.map(_ => ' '))
    board(0)(0) = 'R'
    board(0)(7) = 'R'
    board(0)(1) = 'N'
    board(0)(6) = 'N'
    board(0)(2) = 'B'
    board(0)(5) = 'B'
    board(0)(3) = 'Q'
    board(0)(4) = 'K'
    board(7)(0) = 'r'
    board(7)(7) = 'r'
    board(7)(1) = 'n'
    board(7)(6) = 'n'
    board(7)(2) = 'b'
    board(7)(5) = 'b'
    board(7)(3) = 'q'
    board(7)(4) = 'k'
    board(1) = board(1) map (_ => 'P')
    board(6) = board(6) map (_ => 'p')
  }

  /**
   * Converts boolean value to 0/1 values
   * true to 1
   * false to 0
   *
   * @param bool boolean value to convert
   * @return 0 or 1 value depending on bool value
   */
  private def boolToInt(bool: Boolean): Int = if (bool) 1 else 0

  /**
   * Function to find position of king.
   * Simulates for loop using recursion.
   *
   * @param i row iterator
   * @param j column iterator
   * @param state State of board composed of pieces on board and player turn.
   * @return position of king as tuple (row number, column number) , returns (-1, -1) if not found (unlikely case)
   */
  @tailrec
  private def findKing(i: Int, j:Int, state: State): (Int, Int) = {
    if (j > 7)
      findKing(i+1, 0, state)
    else if (i > 7)
      (-1, -1)
    else if (state.board(i)(j).toInt == 'K'.toInt + 32*boolToInt(state.turn))
      (i, j)
    else findKing(i, j+1, state)
  }

  /**
   * Function to check if the player's king is in check.
   * Simulates for loop using recursion.
   *
   * @param i row iterator integer
   * @param j column iterator integer
   * @param kingPos position of king on board as tuple (row number, column number)
   * @param state State of board composed of pieces on board and player turn.
   * @return <code>Boolean</code> true if king is in check, false if king not in check.
   */
  @tailrec
  private def testCheck(i: Int, j:Int, kingPos: (Int, Int), state: State): Boolean = {
    if (j > 7)
      testCheck(i+1, 0, kingPos, state)
    else if (i > 7)
      false
    else if (validateMove(Array((i, j), kingPos), state))
      true
    else testCheck(i, j+1, kingPos, state)
  }

  /**
   * Function to check if destination of movement of piece is blocked by other friendly piece.
   *
   * @param move Array of tuple of (row number, column number) describing move.
   * @param state State of board composed of pieces on board and player turn.
   * @return <code>Boolean</code> true if move is clear, false if move is blocked
   */
  private def validateDestination(move: Array[(Int, Int)], state:State): Boolean = {
    val withinBounds = move(1)._1 > -1 && move(1)._2 > -1 && move(1)._1 < 8 && move(1)._2 < 8
    withinBounds && (state.board(move(1)._1)(move(1)._2).toInt - 32*boolToInt(state.turn) > 82 || state.board(move(1)._1)(move(1)._2).toInt - 32*boolToInt(state.turn) < 66)
  }

  /**
   * Function to check if destination of movement of piece is blocked by any piece.
   *
   * @param space tuple of (row number, column number) describing square position.
   * @param state State of board composed of pieces on board and player turn.
   * @return <code>Boolean</code> true if move is clear, false if move is blocked
   */
  private def isValidSpace(space: (Int, Int), state:State): Boolean = {
    val withinBounds = space._1 > -1 && space._2 > -1 && space._1 < 8 && space._2 < 8
    withinBounds && state.board(space._1)(space._2) == ' '
  }

  /**
   * Function to check if path of movement of piece is blocked by other friendly piece.
   * Simulates moving a piece square by square.
   *
   * @param move Array of tuple of (row number, column number) describing move.
   * @param decrement Tuple of (row number, column number) describing unit vector of piece move.
   * @param state State of board composed of pieces on board and player turn.
   * @return <code>Boolean</code> true if move is clear, false if move is blocked
   */
  @tailrec
  private def validatePath(move: Array[(Int, Int)], decrement: (Int, Int), state:State): Boolean = {
    if (move(0) == move(1)) true
    else isValidSpace(move(1), state) && validatePath(Array(move(0), (move(1)._1 - decrement._1, move(1)._2 - decrement._2)), decrement, state)
  }

  /**
   * Function to validate movement of rook piece.
   *
   * @param move Array of tuple of (row number, column number) describing move.
   * @return <code>Boolean</code> true if move is legal, false if move is illegal
   */
  private def validateKing(move: Array[(Int, Int)]): Boolean = {
    val verticalCheck = Math.abs(move(1)._2 - move(0)._2) == 1 || Math.abs(move(1)._2 - move(0)._2) == 0
    val horizontalCheck = Math.abs(move(1)._1 - move(0)._1) == 1 || Math.abs(move(1)._1 - move(0)._1) == 0
    verticalCheck && horizontalCheck
  }

  /**
   * Function to validate movement of rook piece.
   *
   * @param move Array of tuple of (row number, column number) describing move.
   * @return <code>Boolean</code> true if move is legal, false if move is illegal
   */
  private def validateRook(move: Array[(Int, Int)]): Boolean = (Math.abs(move(1)._1 - move(0)._1) == 0 && Math.abs(move(1)._2 - move(0)._2) > 0) || (Math.abs(move(1)._1 - move(0)._1) > 0 && Math.abs(move(1)._2 - move(0)._2) == 0)

  /**
   * Function to validate movement of knight piece.
   *
   * @param move Array of tuple of (row number, column number) describing move.
   * @return <code>Boolean</code> true if move is legal, false if move is illegal
   */
  private def validateKnight(move: Array[(Int, Int)]): Boolean = (Math.abs(move(1)._1 - move(0)._1) == 1 && Math.abs(move(1)._2 - move(0)._2) == 2) || (Math.abs(move(1)._1 - move(0)._1) == 2 && Math.abs(move(1)._2 - move(0)._2) == 1)

  /**
   * Function to validate movement of bishop piece.
   *
   * @param move Array of tuple of (row number, column number) describing move.
   * @return <code>Boolean</code> true if move is legal, false if move is illegal
   */
  private def validateBishop(move: Array[(Int, Int)]): Boolean = Math.abs(move(1)._1 - move(0)._1) == Math.abs(move(1)._2 - move(0)._2)

  /**
   * Function to validate movement of queen piece.
   *
   * @param move Array of tuple of (row number, column number) describing move.
   * @return <code>Boolean</code> true if move is legal, false if move is illegal
   */
  private def validateQueen(move: Array[(Int, Int)]): Boolean = validateRook(move) || validateBishop(move)

  /**
   * Function to validate movement of pawn piece.
   *
   * @param move Array of tuple of (row number, column number) describing move.
   * @return <code>Boolean</code> true if move is legal, false if move is illegal
   */
  private def validatePawn(move: Array[(Int, Int)], state: State): Boolean = {
    val whiteBasicMove = state.turn && ((move(0)._1 - move(1)._1 == 2 && move(0)._1 == 6) || (move(0)._1 - move(1)._1 == 1 )) && move(0)._2 == move(1)._2 && state.board(move(1)._1)(move(1)._2) == ' '
    val blackBasicMove = !state.turn && ((move(1)._1 - move(0)._1 == 2 && move(0)._1 == 1) || (move(1)._1 - move(0)._1 == 1 )) && move(0)._2 == move(1)._2 && state.board(move(1)._1)(move(1)._2) == ' '
    val whiteCapture = state.turn && (move(0)._1 - move(1)._1 == 1) && (Math.abs(move(0)._2 - move(1)._2) == 1) && (state.board(move(1)._1)(move(1)._2).toInt <= 82 && state.board(move(1)._1)(move(1)._2).toInt >= 66)
    val blackCapture = !state.turn && (move(1)._1 - move(0)._1 == 1) && (Math.abs(move(0)._2 - move(1)._2) == 1) && (state.board(move(1)._1)(move(1)._2).toInt <= 114 && state.board(move(1)._1)(move(1)._2).toInt >= 98)
    whiteBasicMove || blackBasicMove || whiteCapture || blackCapture
  }

  /**
   * Function to check if a pawn can be promoted.
   *
   * @param move Array of tuple of (row number, column number) describing move.
   * @param state State of board composed of pieces on board and player turn.
   * @return <code>Boolean</code> true if piece is eligible for promotion, false if not.
   */
  private def canPromote(move: Array[(Int, Int)], state: State): Boolean = {
    if (state.turn && move(1)._1 == 0) true
    else if (!state.turn && move(1)._1 == 7) true
    else false
  }

  /**
   * Function to promote a pawn when it reaches end of board.
   * NOTE: Due to engine limitations, pawn is automatically promoted into queen.
   *
   * @param position Board position where pawn is located represented by tuple (row number, column number)
   * @param state State of board composed of pieces on board and player turn.
   */
  private def promote(position: (Int, Int), state:State): Unit = {
    if (state.turn) state.board(position._1)(position._2) = 'q'
    else state.board(position._1)(position._2) = 'Q'
  }

  /**
   * Function to validate player's move follows chess rules.
   *
   * @param move Array of tuple of (row number, column number) describing move.
   * @param state State of board composed of pieces on board and player turn.
   * @return <code>Boolean</code> true if move is legal, false if move is illegal
   */
  private def validateMove(move: Array[(Int, Int)], state:State) = {
    val decrement = (
      if (move(1)._1 - move(0)._1 != 0) (move(1)._1 - move(0)._1)/Math.abs(move(1)._1 - move(0)._1) else 0,
      if (move(1)._2 - move(0)._2 != 0) (move(1)._2 - move(0)._2)/Math.abs(move(1)._2 - move(0)._2) else 0
    )

    state.board(move(0)._1)(move(0)._2) match {
      case ' ' => false
      case x if x.toInt - 32*boolToInt(state.turn) > 82 || x.toInt - 32*boolToInt(state.turn) < 66 => false
      case x if x.toLower == 'k' => validateDestination(move, state) && validateKing(move)
      case x if x.toLower == 'q' => validateDestination(move, state) && validatePath(Array(move(0), (move(1)._1 - decrement._1, move(1)._2 - decrement._2)), decrement, state) && validateQueen(move)
      case x if x.toLower == 'n' => validateDestination(move, state) && validateKnight(move)
      case x if x.toLower == 'r' => validateDestination(move, state) && validatePath(Array(move(0), (move(1)._1 - decrement._1, move(1)._2 - decrement._2)), decrement, state) && validateRook(move)
      case x if x.toLower == 'b' => validateDestination(move, state) && validatePath(Array(move(0), (move(1)._1 - decrement._1, move(1)._2 - decrement._2)), decrement, state) && validateBishop(move)
      case x if x.toLower == 'p' => validateDestination(move, state) && validatePath(Array(move(0), (move(1)._1 - decrement._1, move(1)._2 - decrement._2)), decrement, state) && validatePawn(move, state)
      case _ => throw new RuntimeException("Undefined board state")
    }
  }

  /**
   * Function to perform move input by player, performs all necessary checks calling other functions
   *
   * @param input player input as <code>String</code>.
   * @param state State of board composed of pieces on board and player turn.
   * @return <code>Boolean</code> true if move is legal, false if move is illegal.
   */
  private def correctMove(input: String, state: State): Boolean = {
    if (!validateInput(input)) false
    else {
      val move = parseInput(input)
      if (validateMove(move, state)) {
        val srcPiece = state.board(move(0)._1)(move(0)._2)
        val destPiece = state.board(move(1)._1)(move(1)._2)
        if (state.board(move(0)._1)(move(0)._2) == 'p' || state.board(move(0)._1)(move(0)._2) == 'P')
          if (canPromote(move, state)) promote(move(0), state)
        state.board(move(1)._1)(move(1)._2) = state.board(move(0)._1)(move(0)._2)
        state.board(move(0)._1)(move(0)._2) = ' '
        val kingPos = findKing(0, 0, state)
        state.turn = !state.turn
        val inCheck = testCheck(0, 0, kingPos, state)
        state.turn = !state.turn
        if (inCheck) {
          state.board(move(0)._1)(move(0)._2) = srcPiece
          state.board(move(1)._1)(move(1)._2) = destPiece
          false
        } else {
          true
        }
      } else {
        false
      }
    }
  }

  /**
   * Function to verify the input can be a chess move.
   *
   * @param input <code>String</code> containing player move.
   * @return <code>Boolean</code> true if move can be correct, false if input cannot be parsed into possible move.
   */
  private def validateInput(input: String): Boolean =  {
    val move = parseInput(input)
    val outOfBounds = move map (x => x._1 < 0 || x._1 > 7 || x._2 < 0 || x._2 > 7)
    val negative = input.contains('-')
    if (outOfBounds(0) || outOfBounds(1) || negative) return false
    true
  }

  /**
   * Function to parse string input into array of tuple (row number, column number).
   *
   * @param input user input passed as <code>String</code>
   * @return Array of 2 tuples, each tuple (row number, column number).
   */
  private def parseInput(input: String): Array[(Int, Int)] = {
    val modifiedInput = input.trim + "00 00"
    modifiedInput split " " filter (x => !x.isBlank) map (x => (7 - (x(1).toInt - '1'.toInt), x(0).toLower.toInt - 'a'.toInt))
  }
}
