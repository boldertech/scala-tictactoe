//package sample

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * Created by brian on 8/9/16.
  */
object TicTacToe {

  case class Row(left: Option[PlayerChoice], middle: Option[PlayerChoice], right: Option[PlayerChoice]) {
    def hasWinner: Boolean = {
      left.isDefined && left == middle && middle == right
    }

    def isFull: Boolean = {
      left.isDefined && !middle.isEmpty && !right.isEmpty
    }
  }

  case class Board(top: Row, center: Row, bottom: Row) {
    def print() = {
      printCol()
      printRow(top)
      printLine()
      printRow(center)
      printLine()
      printRow(bottom)
      printCol()
    }

    def printRow(row: Row) = {
      println("\t  " + getPlayerChoiceOutput(row.left) + "  |  " +
        getPlayerChoiceOutput(row.middle) + "  |  " + getPlayerChoiceOutput(row.right))
    }

    def getPlayerChoiceOutput(choice: Option[PlayerChoice]): String = {
      choice match {
        case Some(x) => x.toString
        case None => " "
      }
    }

    def printLine() = {
      println("\t-----+-----+-----")
    }

    def printCol() = {
      println("\t     |     |")
    }

    def hasWinner: Boolean = {
      // see if there is 3 across
      (top.hasWinner || center.hasWinner || bottom.hasWinner) ||
        (top.left.isDefined && top.left == center.left && top.left == bottom.left) ||
        (top.middle.isDefined && top.middle == center.middle && top.middle == bottom.middle) ||
        (top.right.isDefined && top.right == center.right && top.right == bottom.right) ||
        (top.left.isDefined && top.left == center.middle && top.left == bottom.right) ||
        (bottom.left.isDefined && bottom.left == center.middle && bottom.left == top.right)
    }

    /** Are there any more available moves? */
    def isFull: Boolean = {
      top.isFull && center.isFull && bottom.isFull
    }
  }

  sealed trait PlayerChoice

  case object X extends PlayerChoice

  case object O extends PlayerChoice


  sealed trait UserError

  case object InvalidMove extends UserError

  case object GameOver extends UserError

  sealed trait UserInput

  object topLeft extends UserInput

  object topMiddle extends UserInput

  object topRight extends UserInput

  object centerLeft extends UserInput

  object centerMiddle extends UserInput

  object centerRight extends UserInput

  object bottomLeft extends UserInput

  object bottomMiddle extends UserInput

  object bottomRight extends UserInput

  object quit extends UserInput

  object invalid extends UserInput

  def checkMove(attemptedMoveToPosition: Option[PlayerChoice], player: Option[PlayerChoice], board: Board): Either[UserError, Board] = {
    if (attemptedMoveToPosition.isEmpty) {
      // the position to move is valid

      // see if after the move the player won
      if (board.hasWinner) {
        board.print
        println("Congratulations, " + player.get + ", you have won!")
        Left(GameOver)
      } else if (board.isFull) {
        board.print
        println("Cats - it is a tie")
        Left(GameOver)
      } else {
        Right(board)
      }
    } else {
      Left(InvalidMove)
    }
  }

  def update(userInput: UserInput, playerChoice: Option[PlayerChoice], board: Board): Either[UserError, Board] = {
    userInput match {
      case `topLeft` => checkMove(board.top.left, playerChoice, board.copy(top = board.top.copy(left = playerChoice)))
      case `topMiddle` => checkMove(board.top.middle, playerChoice, board.copy(top = board.top.copy(middle = playerChoice)))
      case `topRight` => checkMove(board.top.right, playerChoice, board.copy(top = board.top.copy(right = playerChoice)))
      case `centerLeft` => checkMove(board.center.left, playerChoice, board.copy(center = board.center.copy(left = playerChoice)))
      case `centerMiddle` => checkMove(board.center.middle, playerChoice, board.copy(center = board.center.copy(middle = playerChoice)))
      case `centerRight` => checkMove(board.center.right, playerChoice, board.copy(center = board.center.copy(right = playerChoice)))
      case `bottomLeft` => checkMove(board.bottom.left, playerChoice, board.copy(bottom = board.bottom.copy(left = playerChoice)))
      case `bottomMiddle` => checkMove(board.bottom.middle, playerChoice, board.copy(bottom = board.bottom.copy(middle = playerChoice)))
      case `bottomRight` => checkMove(board.bottom.right, playerChoice, board.copy(bottom = board.bottom.copy(right = playerChoice)))
      case `quit` => Left(GameOver)
      case `invalid` => Left(InvalidMove)
    }
  }

  def getInput: UserInput = {
    println("Enter your move: ")
    val move = StdIn.readChar()
    def result = move match {
      case '1' => topLeft
      case '2' => topMiddle
      case '3' => topRight
      case '4' => centerLeft
      case '5' => centerMiddle
      case '6' => centerRight
      case '7' => bottomLeft
      case '8' => bottomMiddle
      case '9' => bottomRight
      case 'q' => quit
      case _ => invalid
    }
    result
  }

  def showError(err: UserError): Unit = {
    println(err)
  }

  @tailrec
  def loop(playerChoice: Option[PlayerChoice], board: Board): Unit = {
    println("It is " + playerChoice.get + "'s turn")
    val userInput = getInput
    update(userInput, playerChoice, board) match {
      case Left(GameOver) => println("Game Over")
      case Left(InvalidMove) => {
        println("invalid move")
        board.print
        loop(playerChoice, board)
      }
      case Right(updatedBoard) =>
        updatedBoard.print
        playerChoice match {
          case Some(X) => loop(Some(O), updatedBoard)
          case Some(O) => loop(Some(X), updatedBoard)
          case None => println("unexpected input"); loop(playerChoice, updatedBoard)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    println("Tic-tac-toe - Welcome!")
    println("The boxes are numbered 1 - 9 from top left to bottom right. Enter the number for the location you wish to play.")
    println("X goes first.")
    val board = Board(
      Row(None, None, None),
      Row(None, None, None),
      Row(None, None, None))

    loop(Some(X), board)
  }
}
