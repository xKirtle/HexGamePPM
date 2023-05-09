import Cells._
import GameState.swapPlayer
import IOUtils._

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object TextUserInterface extends App {
  private val boardSize: Int = readPositiveInteger(minValue = 3, prompt = "Please enter the board size (minimum 3): ")
  private val initialGameState: GameState = GameState.createNewGameState(boardSize)
  private val initialMoveGenerator: MoveGenerator = MoveGenerator.createNewMoveGenerator

  @tailrec
  private def mainMenu(gameState: GameState, moveGenerator: MoveGenerator, player: Cell): Unit = {
    // Check for win on previous move
    val prevPlayer = swapPlayer(player)
    val winningPath = gameState.getContiguousLineDFS(prevPlayer)

    if (winningPath.nonEmpty) {
      println(s"$prevPlayer wins!")
      val endBoard = gameState.markWinningPath(winningPath, Winner).board
      IOUtils.printBoard(endBoard)

      return
    }
    else if (gameState.isBoardFull) {
      println("Board is full. Game ends in a draw!")
      IOUtils.printBoard(gameState.board)
    }

    println
    println(s"1. Make a move as the [$player] player")
    println(s"2. Make a random move as the [$player] player")
    println(s"3. Undo player [$player]'s last move")
    println(s"4. Quick AI vs AI game")
    println("5. Quit")
    println
    print("Select an option: ")

    val input = readLine()

    input match {
      case "1" =>
        if (gameState.moveHistory.isEmpty)
          IOUtils.printBoard(gameState.board)
          
        val move = getPlayerMove(gameState)
        val newGameState = gameState.play(player, move._1, move._2)
        IOUtils.printBoard(newGameState.board)

        mainMenu(newGameState, moveGenerator, swapPlayer(player))

      case "2" =>
        val (randomMove, newMoveGenerator) = moveGenerator.betterRandomMove(gameState)
        val newGameState = gameState.play(player, randomMove._1, randomMove._2)
        IOUtils.printBoard(newGameState.board)

        mainMenu(newGameState, newMoveGenerator, swapPlayer(player))

      case "3" =>
        val newGameState = gameState.undoPlay(amount = 1)
        IOUtils.printBoard(newGameState.board)

        mainMenu(newGameState, moveGenerator, swapPlayer(player))

      case "4" => 
        GameLoop.main(Array(gameState.board.length.toString))

      case "5" =>
        println("Exiting program")

      case _ =>
        try {
          val _ = input.toInt
          println("Invalid option. Please use a value between [1-5].")
        }
        catch {
          case _: NumberFormatException =>
            println("Invalid input. Please enter a valid number.")
        }

        mainMenu(gameState, moveGenerator, player)
    }
  }

  mainMenu(initialGameState, initialMoveGenerator, Red)
}