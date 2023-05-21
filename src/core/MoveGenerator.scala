package core

import core.Cells.{Blue, Cell, Red}
import core.GameState.{getNeighbours, getStartPosition}

import scala.util.Random

case class MoveGenerator(seed: Long = Random.nextLong) {
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

  def randomMove(board: Board): (Position, MoveGenerator) = {
    val emptyCells = board.getEmptyCellsPositions
    val (index, nextRandom) = nextInt(emptyCells.length)

    (emptyCells(index), nextRandom)
  }

  private def getLastMoveForPlayer(moveHistory: List[(Cell, Int, Int)], player: Cell): Option[(Int, Int)] = {
    val playerMoves = moveHistory.filter { case (cell, _, _) => cell == player }
    playerMoves.headOption.map { case (_, row, col) => (row, col) }
  }

  // Gets all the best coordinates to play and plays a random one (no preference to the ones that get closer to objective)
  def betterRandomMove(gameState: GameState): (Position, MoveGenerator) = {
    val player = gameState.currentPlayer
    val board = gameState.board

    val neighbouringCells = gameState.moveHistory.collect {
      case (position, moveHistoryPlayer) if moveHistoryPlayer == player => getNeighbours(position, moveHistoryPlayer)
    }.flatten.toSet

    val emptyCellsPositions = board.getEmptyCellsPositions
    val emptyNeighbourCellsPositions = emptyCellsPositions.filter(neighbouringCells.contains)

    val startPositions = getStartPosition(player, board, populated = false)
    val emptyStartPositions = emptyCellsPositions.filter(startPositions.contains)

    // If there are no valid neighbours, use start positions or any random neighbouring empty cell
    val validCells = (emptyNeighbourCellsPositions.isEmpty, emptyStartPositions.isEmpty) match {
      case (true, true) => emptyCellsPositions
      case (true, false) => emptyStartPositions
      case _ => emptyNeighbourCellsPositions
    }

    val (index, newMoveGenerator) = nextInt(validCells.length)

    (validCells(index), newMoveGenerator)
  }

  def weightedRandomMove(gameState: GameState): (Position, MoveGenerator) = {
    val player = gameState.currentPlayer
    val board = gameState.board

    val maxBoardIndex = gameState.board.size - 1

    val target = player match {
      case Red => (maxBoardIndex, maxBoardIndex)
      case Blue => (maxBoardIndex, 0)
    }

    // Manhattan distance heuristic
    def heuristic(position: Position, targetRow: Int, targetCol: Int): Int =
      math.abs(position.x - targetRow) + math.abs(position.y - targetCol)

    val neighboringCells = gameState.moveHistory.flatMap {
      case (position, _) => getNeighbours(position, player)
    }.toSet

    val scoredNeighboringCells = neighboringCells.map { position =>
      (position, heuristic(position, target._1, target._2))
    }

    val bestMove = scoredNeighboringCells.minBy(_._2)._1

    (bestMove, this)
  }
}
