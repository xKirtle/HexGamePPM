package core

import core.Cells.{Cell, Empty}

// TODO: core.Board pode ser uma case class em vez do type? || type core.Board = List[List[Cells.Cell]]

// private para obrigar a usar o apply
case class Board private (size: Int, cells: List[List[Cell]]) {
  def isFull: Boolean = getEmptyCellsPositions == List.empty

  def isValidPosition(position: Position): Boolean = {
    // destructuring into a core.Position object
    val Position(x, y) = position
    x >= 0 && x < size && y >= 0 && y < size
  }

  def getEmptyCellsPositions: List[Position] = {
    cells.zipWithIndex.flatMap { 
      case (row, rowIndex) =>
        row.zipWithIndex.collect {
          case (Empty, columnIndex) => Position(rowIndex, columnIndex)
        }
    }
  }

  def updateCellsAt(cell: Cell, coords: Position*): Board = {
    val newCells = cells.zipWithIndex.map {
      case (currentRow, rowIndex) =>
        currentRow.zipWithIndex.map {
          case (currentCell, columnIndex) =>
            if (coords.contains(Position(rowIndex, columnIndex))) cell
            else currentCell
        }
    }
    
    Board(size, newCells);
  }

  def getCellAt(position: Position): Cell = {
    val Position(x, y) = position
    cells(x)(y)
  }

  def markWinningPath(winningPath: List[Position], winningCell: Cell): Board = {
    updateCellsAt(winningCell, winningPath: _*)
  }
}

object Board {
  def apply(size: Int): Board = new Board(size, List.fill(size)(List.fill(size)(Empty)))
}

// vai chamar o apply do companion object
//core.Board(size = 25)