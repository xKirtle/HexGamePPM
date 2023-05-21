package core

import core.Cells.{Red, Winner}
import core.GameState.{getContiguousLineDFS, swapPlayer}

import scala.annotation.tailrec

object GameLoop extends App {
  private val boardSize: Int = args(0).toInt
  private val initialGameState: GameState = GameState.createNewGameState(boardSize, Red)
  private val initialMoveGenerator: MoveGenerator = MoveGenerator()

  @tailrec
  private def gameLoop(gameState: GameState, moveGenerator: MoveGenerator): Unit = {
    val prevPlayer = swapPlayer(gameState.currentPlayer)
    val winningPath = getContiguousLineDFS(prevPlayer, gameState.board)

    if (winningPath.nonEmpty) {
      println(s"$prevPlayer wins!")
      val endBoard = gameState.board.markWinningPath(winningPath, Winner)
      IOUtils.printBoard(endBoard)
    }
    else if (gameState.isBoardFull) {
      println("Board is full. Game ends in a draw!")
      IOUtils.printBoard(gameState.board)
    }
    else {
      val (randomMove, newMoveGenerator) = moveGenerator.randomMove(gameState.board)
      val newGameState = gameState.play(randomMove)

      gameLoop(newGameState, newMoveGenerator)
    }
  }

  gameLoop(initialGameState, initialMoveGenerator)
}
