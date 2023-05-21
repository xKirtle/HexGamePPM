import core.{Cells, GameState, Position}
import javafx.fxml.FXML
import javafx.fxml.Initializable
import javafx.scene.input.MouseEvent
import javafx.scene.layout.GridPane
import javafx.scene.{Group, Node, paint}
import javafx.scene.paint.Color
import javafx.scene.shape.{LineTo, MoveTo, Path, Polygon}

import java.net.URL
import java.util.ResourceBundle

class GameController extends Initializable {

  @FXML private var gameBoard: GridPane = _
  @FXML private var rootGroup: Group = _

  private val boardSize: Int = 5;
  private val numRows: Int = boardSize
  private val numColumns: Int = boardSize

  private val hexagonRadius: Double = 25
  
  private var gameState: GameState = GameState.createNewGameState(boardSize, Cells.Red)

  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    drawHexGameBoard()
  }

  private def drawHexGameBoard(): Unit = {
    for (row <- 0 until numRows) {
      for (col <- 0 until numColumns) {
        val hexagonCopy: Polygon = createHexagon(Position(row, col))

        // Shift every second row to create the honeycomb pattern and add padding to the left to create a hex board
        if (row % 2 == 1) {
          hexagonCopy.setTranslateX(hexagonRadius * Math.sqrt(3) * (col + Math.ceil(row / 2 + 0.5)))
        } else {
          hexagonCopy.setTranslateX(hexagonRadius * Math.sqrt(3) * (col + Math.floor(row / 2)) + hexagonRadius * Math.sqrt(3) / 2)
        }

        hexagonCopy.setTranslateY(hexagonRadius * 3 / 2 * row) // Adjust the y-coordinate
        hexagonCopy.setOnMouseClicked((event: MouseEvent) => { handleHexagonClick(hexagonCopy) })

        gameBoard.getChildren.add(hexagonCopy) // Add the hexagon to the game board
      }
    }
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
    newHexagon.setId(positionToIndex(position).toString)
    newHexagon
  }

  private def handleHexagonClick(hexagon: Polygon): Unit = {
    // Position already played
    if (hexagon.getFill != Color.WHITE) return

    println("Hexagon clicked: " + hexagon.getId)

    val position = indexToPosition(hexagon.getId.toInt)

    val player = gameState.currentPlayer
    hexagon.setFill(if (player == Cells.Red) Color.RED else Color.BLUE)
    gameState = gameState.play(position)

    val winningPath = GameState.getContiguousLineDFS(player, gameState.board)
    if (winningPath.nonEmpty) {
      val winningLine = new Path()

      for (i <- winningPath.indices) {
        val position: Position = winningPath(i)
        val index: Int = positionToIndex(position)

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
          case _ =>
        }
      }

      // Set line color and width
      winningLine.setStroke(Color.YELLOW)
      winningLine.setStrokeWidth(3)

      // Add the winningLine to the gameBoard
      rootGroup.getChildren.add(winningLine)
    }
  }

  private def positionToIndex(position: Position): Int = numRows * position.x + position.y
  
  private def indexToPosition(index: Int): Position = Position(index / numRows, index % numRows)
}