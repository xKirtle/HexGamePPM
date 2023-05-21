import core.Cells._
import core.GameState.{getContiguousLineDFS, swapPlayer}
import core.{GameLoop, GameState, IOUtils, MoveGenerator}
import core.IOUtils._

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object TextUserInterface extends App {
  private val boardSize: Int = readPositiveInteger(minValue = 3, prompt = "Please enter the board size (minimum 3): ")
  private val initialGameState: GameState = GameState.createNewGameState(boardSize, Red)
  private val initialMoveGenerator: MoveGenerator = MoveGenerator()

  @tailrec
  private def mainMenu(gameState: GameState, moveGenerator: MoveGenerator): Unit = {
    // Check for win on previous move
    val prevPlayer = swapPlayer(gameState.currentPlayer)
    val winningPath = getContiguousLineDFS(prevPlayer, gameState.board)

    if (winningPath.nonEmpty) {
      println(s"$prevPlayer wins!")
      val endBoard = gameState.board.markWinningPath(winningPath, Winner)
      IOUtils.printBoard(endBoard)

      return
    }
    else if (gameState.isBoardFull) {
      println("Board is full. Game ends in a draw!")
      IOUtils.printBoard(gameState.board)
    }

    print(
      s"""
         |1. Make a move as the [${gameState.currentPlayer}] player
         |2. Make a random move as the [${gameState.currentPlayer}] player
         |3. Undo player [${gameState.currentPlayer}]'s last move
         |4. Quick AI vs AI game
         |5. Quit
         |
         |Select an option: """.stripMargin)
    
    readLine() match {
      case "1" =>
        if (gameState.moveHistory.isEmpty)
          IOUtils.printBoard(gameState.board)
          
        val move = getPlayerMove(gameState)
        val newGameState = gameState.play(move)
        IOUtils.printBoard(newGameState.board)

        mainMenu(newGameState, moveGenerator)

      case "2" =>
        val (randomMove, newMoveGenerator) = moveGenerator.betterRandomMove(gameState)
        val newGameState = gameState.play(randomMove)
        IOUtils.printBoard(newGameState.board)

        mainMenu(newGameState, newMoveGenerator)

      case "3" =>
        val newGameState = gameState.undoPlay(amount = 1)
        IOUtils.printBoard(newGameState.board)

        mainMenu(newGameState, moveGenerator)

      case "4" => 
        GameLoop.main(Array(gameState.board.size.toString))

      case "5" =>
        println("Exiting program")

      case input =>
        println(s"Invalid option $input. Please use a value between [1-5].")
        
        mainMenu(gameState, moveGenerator)
    }
  }

  mainMenu(initialGameState, initialMoveGenerator)
}