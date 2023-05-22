package core

import core.Cells.{Blue, Cell, Empty, Red}
import core.GameState.swapPlayer

import scala.annotation.tailrec

case class GameState(board: Board, moveHistory: List[(Position, Cell)], currentPlayer: Cell) {
  def isBoardFull: Boolean = board.isFull

  def isValidMove(position: Position): Boolean = {
    board.isValidPosition(position) && board.getCellAt(position) == Empty
  }

  def play(position: Position): GameState = {
    val newBoard = board.updateCellsAt(currentPlayer, position)
    val newMoveHistory = (position, currentPlayer) :: moveHistory

    GameState(newBoard, newMoveHistory, swapPlayer(currentPlayer))
  }

  def undoPlay(amount: Int): GameState = {
    if (amount <= 0) return this
      
    val (movesToUndo, newMoveHistory) = moveHistory.splitAt(amount)
    val moves = movesToUndo.map {
      case (position, _) => position
    }

    val newBoard = board.updateCellsAt(Empty, moves: _*)
    val newPlayer = if (amount % 2 == 0) currentPlayer else swapPlayer(currentPlayer)
    GameState(newBoard, newMoveHistory, newPlayer)
  }
}

object GameState {
  def createNewGameState(boardSize: Int, firstPlayer: Cell): GameState = {
    // Ensuring firstPlayer is Blue, if an undesired Cell value is passed, or Red
    val actualFirstPlayer = swapPlayer(swapPlayer(firstPlayer))
    GameState(Board(boardSize), List.empty, actualFirstPlayer)
  }

  def swapPlayer(player: Cell): Cell = {
    if (player == Red) Blue else Red
  }

  def getStartPosition(player: Cell, board: Board, populated: Boolean): List[Position] = {
    val cellType = if (populated) player else Empty

    player match {
      case Red => board.cells.indices.collect {
        case i if board.cells(i).head == cellType => Position(i, 0)
      }.toList
      case Blue => board.cells.head.indices.collect {
        case i if board.cells.head(i) == cellType => Position(0, i)
      }.toList
      case _ => List()
    }
  }

  def getNeighbours(position: Position, player: Cell): List[Position] = player match {
    case Red => List(position.right, position.bottomRight, position.topRight)
    case Blue => List(position.bottomRight, position.bottomLeft, position.right, position.left)
    case _ => List.empty
  }

  def getContiguousLineDFS(player: Cell, board: Board): List[Position] = {
    val size = board.size 

    @tailrec
    def dfs(stack: List[(Position, List[Position])], visitedCoords: Set[Position], cache: Map[Position, List[Position]]): (List[Position], Map[Position, List[Position]]) = {
      stack match {
        // If the stack is empty, return the cache
        case Nil => (Nil, cache)

        // If the stack is not empty, pop the top position and its associated path
        case (pos, path) :: rest =>
          // If the current position is not valid or does not contain the player's cell, skip it and continue with the rest of the stack
          if (!board.isValidPosition(pos) || board.getCellAt(pos) != player)
            dfs(rest, visitedCoords, cache)

          // If the current position is a winning position for the player, return the path leading to it
          else if ((player == Red && pos.y == size - 1) || (player == Blue && pos.x == size - 1))
            (pos :: path, cache)

          // If the current position is valid and has not been visited before, visit it
          else if (!visitedCoords.contains(pos)) {
            val newVisitedCoords = visitedCoords + pos
            val neighbours = getNeighbours(pos, player)

            // Check the cache to see if the current position has been visited before
            cache.get(pos) match {

              // If the current position is in the cache, skip it and continue with the rest of the stack
              case Some(cachedResult) =>
                dfs(rest, visitedCoords, cache)

              // If the current position is not in the cache, add its neighbours to the stack and update the cache
              case None =>
                val newStack = neighbours.map((_, pos :: path)) ++ rest
                dfs(newStack, newVisitedCoords, cache + (pos -> (pos :: path)))
            }
          }

          // If the current position has been visited before, skip it and continue with the rest of the stack
          else dfs(rest, visitedCoords, cache)
      }
    }

    val startPosition = getStartPosition(player, board, populated = true)
    val initialCache = Map.empty[Position, List[Position]]
    val startStack = startPosition.map((_, Nil))

    val (path, _) = dfs(startStack, Set.empty, initialCache)

    path
  }
}