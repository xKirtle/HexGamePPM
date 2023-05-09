import Cells.{Red, Blue, Cell}

import scala.util.Random

case class MoveGenerator(seed: Long) {
  private def nextInt: (Int, MoveGenerator) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MoveGenerator(newSeed)
    val n = (newSeed >>> 16).toInt
    
    (n, nextRandom)
  }

  private def nextInt(n: Int): (Int, MoveGenerator) = {
    val (rawInt, nextRandom) = nextInt
    val nn = rawInt % n
    
    (if (nn < 0) -nn else nn, nextRandom)
  }

  def randomMove(gameState: GameState): ((Int, Int), MoveGenerator) = {
    val emptyCells = gameState.getEmptyCells
    val (index, nextRandom) = nextInt(emptyCells.length)

    (emptyCells(index), nextRandom)
  }
  
  private def getLastMoveForPlayer(moveHistory: List[(Cell, Int, Int)], player: Cell): Option[(Int, Int)] = {
    val playerMoves = moveHistory.filter { case (cell, _, _) => cell == player }
    playerMoves.headOption.map { case (_, row, col) => (row, col) }
  }
  
  // Gets all the best coordinates to play and plays a random one (no preference to the ones that get closer to objective)
  def betterRandomMove(gameState: GameState): ((Int, Int), MoveGenerator) = {
    val player = gameState.getCurrentPlayer
    
    val neighbouringCells = gameState.moveHistory.collect {
      case (row, col, moveHistoryPlayer) if moveHistoryPlayer == player => gameState.getNeighbours(row, col, moveHistoryPlayer)
    }.flatten.toSet

    val emptyCells = gameState.getEmptyCells
    val emptyNeighbourCells = emptyCells.filter(neighbouringCells.contains)
    
    val startPositions = gameState.getStartPosition(player, populated = false)
    val emptyStartPositions = emptyCells.filter(startPositions.contains)

    // If there are no valid neighbours, use start positions or any random neighbouring empty cell
    val validCells = (emptyNeighbourCells.isEmpty, emptyStartPositions.isEmpty) match {
      case (true, true) => emptyCells
      case (true, false) => emptyStartPositions
      case _ => emptyNeighbourCells
    }

    val (index, newMoveGenerator) = nextInt(validCells.length)

    (validCells(index), newMoveGenerator)
  }
  
  def weightedRandomMove(gameState: GameState): ((Int, Int), MoveGenerator) = {
    val player = if (gameState.moveHistory.head._3 == Red) Blue else Red
    val maxRowIndex = gameState.board.length - 1
    val maxColIndex = gameState.board.head.length - 1

    val target = player match {
      case Red => (maxRowIndex, maxColIndex)
      case Blue => (maxRowIndex, 0)
    }

    // Manhattan distance heuristic
    def heuristic(row: Int, col: Int, targetRow: Int, targetCol: Int): Int =
      math.abs(row - targetRow) + math.abs(col - targetCol)

    val neighboringCells = gameState.moveHistory.flatMap {
      case (row, col, _) => gameState.getNeighbours(row, col, player)
    }.toSet

    val scoredNeighboringCells = neighboringCells.map { case (row, col) =>
      ((row, col), heuristic(row, col, target._1, target._2))
    }

    val bestMove = scoredNeighboringCells.minBy(_._2)._1

    (bestMove, this)
  }
}

object MoveGenerator {
  def createNewMoveGenerator: MoveGenerator = MoveGenerator(Random.nextLong())
}