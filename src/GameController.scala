import core.Position.{indexToPosition, positionToIndex}
import core.{Board, Cells, GameState, MoveGenerator, Position}
import javafx.fxml.FXML
import javafx.fxml.Initializable
import javafx.scene.control.Button
import javafx.scene.input.MouseEvent
import javafx.scene.layout.GridPane
import javafx.scene.{Group, Node}
import javafx.scene.paint.Color
import javafx.scene.shape.{LineTo, MoveTo, Path, Polygon}

import java.net.URL
import java.util.ResourceBundle
import scala.annotation.tailrec

class GameController extends Initializable {

  var isOpponentCPU: Boolean = false
  @FXML private var gameBoard: GridPane = _
  @FXML private var rootGroup: Group = _
  @FXML private var undoButton: Button = _

  private val boardSize: Int = 5
  private val numRows: Int = boardSize
  private val numColumns: Int = boardSize

  private val hexagonRadius: Double = 25
  
  private var gameState: GameState = GameState.createNewGameState(boardSize, Cells.Red)
  private var moveGenerator: MoveGenerator = MoveGenerator()
  private var gameOver: Boolean = false

  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    drawHexGameBoard()
  }

  private def drawHexGameBoard(): Unit = {
    @tailrec
    def drawRow(row: Int): Unit = {
      if (row < numRows) {
        drawColumn(row, 0)
        drawRow(row + 1)
      }
    }

    @tailrec
    def drawColumn(row: Int, col: Int): Unit = {
      if (col < numColumns) {
        val hexagonCopy: Polygon = createHexagon(Position(row, col))

        // Shift every second row to create the honeycomb pattern and add padding to the left to create a hex board
        if (row % 2 == 1) {
          hexagonCopy.setTranslateX(hexagonRadius * Math.sqrt(3) * (col + Math.ceil(row / 2 + 0.5)))
        } else {
          hexagonCopy.setTranslateX(hexagonRadius * Math.sqrt(3) * (col + Math.floor(row / 2)) + hexagonRadius * Math.sqrt(3) / 2)
        }

        hexagonCopy.setTranslateY(hexagonRadius * 3 / 2 * row) // Adjust the y-coordinate
        hexagonCopy.setOnMouseClicked((_: MouseEvent) => {
          handleHexagonClick(hexagonCopy)
        })

        gameBoard.getChildren.add(hexagonCopy) // Add the hexagon to the game board

        drawColumn(row, col + 1)
      }
    }

    drawRow(0)
  }


  private def createHexagon(position: Position): Polygon = {
    val newHexagon: Polygon = new Polygon
    newHexagon.getPoints.addAll(
      0.0, hexagonRadius,
      hexagonRadius * Math.sqrt(3) / 2, hexagonRadius / 2,
      hexagonRadius * Math.sqrt(3) / 2, -hexagonRadius / 2,
      0.0, -hexagonRadius,
      -hexagonRadius * Math.sqrt(3) / 2, -hexagonRadius / 2,
      -hexagonRadius * Math.sqrt(3) / 2, hexagonRadius / 2
    )
    newHexagon.setFill(Color.WHITE) // Set the hexagon fill color
    newHexagon.setStroke(Color.BLACK) // Set the hexagon stroke color
    newHexagon.setStrokeWidth(1) // Set the hexagon stroke width
    newHexagon.setId(positionToIndex(position, numRows).toString)
    newHexagon
  }

  private def handleHexagonClick(hexagon: Polygon): Unit = {
    // Position already played or game over
    if (hexagon.getFill != Color.WHITE || gameOver) return

    val position = indexToPosition(hexagon.getId.toInt, numRows)

    val player = gameState.currentPlayer
    hexagon.setFill(if (player == Cells.Red) Color.RED else Color.BLUE)
    
    gameState = gameState.play(position)

    tryToDisplayWinningPathForPlayer(player)
    
    
    // TODO: Clean this up, put into its own function and call it after handleHexagonClick where we define it?
    if (isOpponentCPU) {
      val cpuPlayer = gameState.currentPlayer
      val (randomMove, newMoveGenerator) = moveGenerator.betterRandomMove(gameState)
      val index = positionToIndex(randomMove, numRows)
      gameBoard.getChildren.get(index) match {
        case hexagon: Polygon => hexagon.setFill(if (cpuPlayer == Cells.Red) Color.RED else Color.BLUE)
        case _ =>
      }
      
      gameState = gameState.play(randomMove)
      moveGenerator = newMoveGenerator
      
      tryToDisplayWinningPathForPlayer(cpuPlayer)
    }
  }
  
  private def tryToDisplayWinningPathForPlayer(player: Cells.Cell): Unit = {
    val winningPath: List[Position] = GameState.getContiguousLineDFS(player, gameState.board)
    if (winningPath.nonEmpty) {
      val winningLine = new Path()
      traverseWinningPath(winningPath, 0, winningLine)

      // Set line color and width
      winningLine.setStroke(Color.YELLOW)
      winningLine.setStrokeWidth(3)

      // Add the winningLine to the gameBoard
      rootGroup.getChildren.add(winningLine)
      
      gameOver = true
    }

    @tailrec
    def traverseWinningPath(winningPath: List[Position], i: Int, winningLine: Path): Unit = {
      if (i < winningPath.length) {
        val index: Int = positionToIndex(winningPath(i), numRows)

        val node: Node = gameBoard.getChildren.get(index)
        node match {
          case winningHexagon: Polygon =>
            // Calculate the center coordinates of the winningHexagon
            val centerX = winningHexagon.getBoundsInParent.getCenterX
            val centerY = winningHexagon.getBoundsInParent.getCenterY

            // Create a new LineTo path element for each hexagon in the winningPath
            if (i == 0) {
              // For the first hexagon, we need to move to the center instead of drawing a line to it
              winningLine.getElements.add(new MoveTo(centerX, centerY))
            } else {
              winningLine.getElements.add(new LineTo(centerX, centerY))
            }

            traverseWinningPath(winningPath, i + 1, winningLine)
          case _ =>
        }
      }
    }
  }
  
  @FXML
  def onUndoClicked(): Unit = {
    val amount = if (isOpponentCPU) 2 else 1
    
    gameState = gameState.undoPlay(amount)
    updateHexagonsWithBoardValues()
  }

  private def updateHexagonsWithBoardValues(): Unit = {
    @tailrec
    def update(position: Position = Position.zero): Unit = {
      val Position(x, y) = position
      
      if (x < numRows) {
        if (y < numColumns) {

          val cell: Cells.Cell = gameState.board.cells(x)(y)
          val index: Int = positionToIndex(position, numRows)
          val node: Node = gameBoard.getChildren.get(index)
          
          node match {
            case hexagon: Polygon =>
              var color: Color = Color.WHITE

              cell match {
                case Cells.Red => color = Color.RED
                case Cells.Blue => color = Color.BLUE
                case _ => color = Color.WHITE
              }

              hexagon.setFill(color)
            case _ =>
          }

          update(position.copy(y = y + 1))
        } else {
          update(Position(x + 1, 0))
        }
      }
    }

    update()
  }
}