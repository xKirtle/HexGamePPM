package core

import core.Cells.{Blue, Cell, Empty, Red, Winner}

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object IOUtils {
  def printBoard(board: Board): Unit = {
    def cellToColoredString(cell: Cell): String = cell match {
      case Red => "🟥"
      case Blue => "🟦"
      case Empty => "⬜"
      case Winner => "🟨"
    }

    def asterisksPerSquare(x: Int): Int = {
      val m = 1.1167
      val b = 0.5
      (m * x + b).round.toInt
    }

    val redAsterisk = Console.RED + "*" + Console.RESET
    val blueAsterisk = Console.BLUE + "*" + Console.RESET
    val numCols = board.cells.head.length

    val header = "X " + s"$blueAsterisk " * asterisksPerSquare(numCols) + "X"

    val rows = board.cells.zipWithIndex.map {
      case (row, index) =>
        val padding = " " * index
        s"$padding $redAsterisk ${row.map(cellToColoredString).mkString("")} $redAsterisk"
    }.mkString("\n")

    val footer = " " * (numCols + 1) + header

    println(s"$header\n$rows\n$footer")
  }

  @tailrec
  def getPlayerMove(gameState: GameState): Position = {
    val prompt = "Please enter your move coordinates separated by a space (ex: '0 11'): "
    val position = readTwoPositiveIntegers(prompt)

    if (gameState.isValidMove(position)) {
      position
    }
    else {
      println("Invalid coordinates or coordinates already used. Please try again.")
      getPlayerMove(gameState)
    }
  }

  private def readTwoPositiveIntegers(prompt: String): Position = {
    val input = readLine(prompt)
    input.split(" ") match {
      case Array(a, b) =>
        try {
          val x = a.toInt
          val y = b.toInt

          if (x >= 0 && y >= 0) Position(x, y)
          else {
            println("Both numbers must be positive.")
            readTwoPositiveIntegers(prompt)
          }
        }
        catch {
          case _: NumberFormatException =>
            println("Invalid input. Please enter two positive integers separated by a space.")
            readTwoPositiveIntegers(prompt)
        }
      case _ =>
        println("Invalid input. Please enter two positive integers separated by a space.")
        readTwoPositiveIntegers(prompt)
    }
  }

  def readPositiveInteger(minValue: Int, prompt: String): Int = {
    val input = readLine(prompt)
    try {
      val x = input.toInt

      if (x >= minValue) x
      else {
        println(s"The integer must be greater than or equal to $minValue.")
        readPositiveInteger(minValue, prompt)
      }
    }
    catch {
      case _: NumberFormatException =>
        println(s"Invalid input. Please enter a single integer greater than or equal to $minValue.")
        readPositiveInteger(minValue, prompt)
    }
  }
}
