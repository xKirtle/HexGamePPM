import Cells._
import GameState.{swapPlayer, getStartPosition, getNeighbours}

// TODO: Include current player in game state? Swap value on play() and undo()
case class GameState(board: Board, moveHistory: List[(Int, Int, Cell)]) {
  def isBoardFull: Boolean = getEmptyCells == List.empty

  def isValidMove(position: Position): Boolean = {
    board.isValidPosition(position) && board.getCellAt(position) == Empty
  }
  
  def play(position: Position): GameState = {
    val newBoard = board.updateCellsAt(currentPlayer, position)
    val newMoveHistory = (position, currentPlayer) :: moveHistory
    
    GameState(newBoard, newMoveHistory, swapPlayer(currentPlayer))
  }
  
  def undoPlay(amount: Int): GameState = {
    val newMoveHistory = moveHistory.drop(amount)
    val moves = moveHistory.take(amount).map {
      case (row, col, _) => (row, col)
    }
    
    val newBoard = board.updateCellsAt(Empty, moves: _*)
    GameState(newBoard, newMoveHistory, swapPlayer(currentPlayer))
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

    def dfs(pos: Position, visitedCoords: Set[Position], cache: Map[Position, List[Position]]): (List[Position], Map[Position, List[Position]]) = {
      if (!board.isValidPosition(pos) || board.getCellAt(pos) != player)
        return (List.empty, cache)

      if (player == Red && pos.y == size - 1) (List(pos), cache)
      else if (player == Blue && pos.x == size - 1) (List(pos), cache)
      else if (!visitedCoords.contains(pos)) {
        val newVisitedCoords = visitedCoords + pos
        val neighbours = getNeighbours(pos, player)

        cache.get(pos) match {
          case Some(cachedResult) => (cachedResult, cache)
          case None =>
            val (pathFound, updatedCache) = neighbours.foldLeft((List.empty[Position], cache)) {
              case ((pathFound, cache), Position(neighbourX, neighbourY)) =>
                if (pathFound.nonEmpty) (pathFound, cache)
                else {
                  val neighbourPos = Position(neighbourX, neighbourY)
                  val (path, newCache) = dfs(neighbourPos, newVisitedCoords, cache)
                  if (path.nonEmpty) (pos :: path, newCache) else (path, newCache)
                }
            }
            val newCache = updatedCache + (pos -> pathFound)
            (pathFound, newCache)
        }
      }
      else (List.empty, cache)
    }

    val startPosition = getStartPosition(player, board, populated = true)
    val initialCache = Map.empty[Position, List[Position]]

    val (path, _) = startPosition.foldLeft((List.empty[Position], initialCache)) {
      case ((pathFound, cache), pos) =>
        if (pathFound.nonEmpty) (pathFound, cache)
        else dfs(pos, Set.empty, cache)
    }

    path
  }
}