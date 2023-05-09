import Cells._
import GameState.{Board, swapPlayer}

// TODO: Include current player in game state? Swap value on play() and undo()
case class GameState(board: Board, moveHistory: List[(Int, Int, Cell)]) {
  def isBoardFull: Boolean = getEmptyCells == List.empty

  def isValidMove(row: Int, col: Int): Boolean = {
    isValidBoardPosition(row, col) && board(row)(col) == Empty
  }

  private def isValidBoardPosition(row: Int, col: Int): Boolean = {
    row >= 0 && row < board.length && col >= 0 && col < board(row).length
  }
  
  def getCurrentPlayer: Cell = swapPlayer(moveHistory.headOption.getOrElse((0, 0, Blue))._3)
  
  def getEmptyCells: List[(Int, Int)] = {
    def traverseAllRows(board: Board, rowIndex: Int): List[(Int, Int)] = board match {
      case head :: tail => traverseRow(head, rowIndex, 0) ++ traverseAllRows(tail, rowIndex + 1)
      case Nil => Nil
    }

    def traverseRow(row: List[Cell], rowIndex: Int, colIndex: Int): List[(Int, Int)] = row match {
      case head :: tail =>
        val current = if (head == Empty) List((rowIndex, colIndex)) else Nil
        current ++ traverseRow(tail, rowIndex, colIndex + 1)
      case Nil => Nil
    }

    traverseAllRows(board, 0)
  }
  
  private def updateCellsAt(coordsList: List[(Int, Int)], cell: Cell): Board = {
    board.zipWithIndex.map {
      case (currentRow, rowIndex) =>
        currentRow.zipWithIndex.map {
          case (currentCell, columnIndex) =>
            if (coordsList.contains((rowIndex, columnIndex))) cell
            else currentCell
        }
    }
  }

  def play(player: Cell, row: Int, col: Int): GameState = {
    val newBoard = updateCellsAt(List((row, col)), player)
    
    GameState(newBoard, (row, col, player) :: moveHistory)
  }
  
  def undoPlay(amount: Int): GameState = {
    val newMoveHistory = moveHistory.drop(amount)
    val moves = moveHistory.take(amount).map {
      case (row, col, _) => (row, col)
    }

    val newBoard = updateCellsAt(moves, Empty)
    GameState(newBoard, newMoveHistory)
  }

  def markWinningPath(winningPath: List[(Int, Int)], winningCell: Cell): GameState = {
    val newBoard = updateCellsAt(winningPath, winningCell)
    GameState(newBoard, moveHistory)
  }

  def getStartPosition(player: Cell, populated: Boolean): List[(Int, Int)] = {
    val cellType = if (populated) player else Empty

    player match {
      case Red => board.indices.filter(i => board(i).head == cellType).map(i => (i, 0)).toList
      case Blue => board.head.indices.filter(i => board.head(i) == cellType).map(i => (0, i)).toList
      case _ => List()
    }
  }


  def getNeighbours(row: Int, col: Int, player: Cell): List[(Int, Int)] = player match {
    // right, bottom right, top right
    case Red => List((row + 1, col), (row, col + 1), (row - 1, col + 1))
    // bottom right, bottom left, right, left
    case Blue => List((row + 1, col), (row + 1, col - 1), (row, col + 1), (row, col - 1))
    case _ => List.empty
  }

  // MEMOIZED
  def getContiguousLineDFS(player: Cell): List[(Int, Int)] = {
    val numRows = board.length
    val numCols = board.head.length

    def dfs(row: Int, col: Int, visitedCoords: Set[(Int, Int)], cache: Map[(Int, Int), List[(Int, Int)]]): (List[(Int, Int)], Map[(Int, Int), List[(Int, Int)]]) = {
      if (!isValidBoardPosition(row, col) || board(row)(col) != player)
        return (List.empty, cache)
      
      if (player == Red && col == numCols - 1) (List((row, col)), cache)
      else if (player == Blue && row == numRows - 1) (List((row, col)), cache)
      else if (!visitedCoords.contains((row, col))) {
        val newVisitedCoords = visitedCoords + ((row, col))
        val neighbours = getNeighbours(row, col, player)

        cache.get((row, col)) match {
          case Some(cachedResult) => (cachedResult, cache)
          case None =>
            val (pathFound, updatedCache) = neighbours.foldLeft((List.empty[(Int, Int)], cache)) {
              case ((pathFound, cache), (rowIndex, colIndex)) =>
                if (pathFound.nonEmpty) (pathFound, cache)
                else {
                  val (path, newCache) = dfs(rowIndex, colIndex, newVisitedCoords, cache)
                  if (path.nonEmpty) ((row, col) :: path, newCache) else (path, newCache)
                }
            }
            val newCache = updatedCache + ((row, col) -> pathFound)
            (pathFound, newCache)
        }
      }
      else (List.empty, cache)
    }

    val startPosition = getStartPosition(player, populated = true)
    val initialCache = Map.empty[(Int, Int), List[(Int, Int)]]

    val (path, _) = startPosition.foldLeft((List.empty[(Int, Int)], initialCache)) {
      case ((pathFound, cache), (row, col)) =>
        if (pathFound.nonEmpty) (pathFound, cache)
        else dfs(row, col, Set.empty, cache)
    }

    path
  }
}

object GameState {
  type Board = List[List[Cells.Cell]]
  
  def createNewGameState(boardSize: Int): GameState = GameState(createEmptyBoard(boardSize), List.empty)
  
  private def createEmptyBoard(boardSize: Int): Board = List.fill(boardSize)(List.fill(boardSize)(Empty))

  def swapPlayer(player: Cell): Cell = {
    if (player == Red) Blue else Red
  }
}