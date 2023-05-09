import Cells._
import GameState.swapPlayer

import scala.annotation.tailrec

private object GameLoop extends App {
  private val boardSize: Int = args(0).toInt
  private val initialGameState: GameState = GameState.createNewGameState(boardSize)
  private val initialMoveGenerator: MoveGenerator = MoveGenerator.createNewMoveGenerator
  
  @tailrec
  private def gameLoop(gameState: GameState, moveGenerator: MoveGenerator, player: Cell): Unit = {
    val winningPath = gameState.getContiguousLineDFS(player)
    
    if (winningPath.nonEmpty) {
      println(s"$player wins!")
      val endBoard = gameState.markWinningPath(winningPath, Winner).board
      IOUtils.printBoard(endBoard)
    }
    else if (gameState.isBoardFull) {
      println("Board is full. Game ends in a draw!")
      IOUtils.printBoard(gameState.board)
    }
    else {
      val (randomMove, newMoveGenerator) = moveGenerator.randomMove(gameState)
      val newGameState = gameState.play(player, randomMove._1, randomMove._2)
      
      gameLoop(newGameState, newMoveGenerator, swapPlayer(player))
    }
  }
  
  gameLoop(initialGameState, initialMoveGenerator, Red)
}
