import javafx.fxml.FXML
import javafx.fxml.Initializable
import javafx.scene.layout.GridPane
import javafx.scene.paint.Color
import javafx.scene.shape.Polygon

import java.net.URL
import java.util.ResourceBundle

class GameController extends Initializable {

  @FXML private var gameBoard: GridPane = _
  @FXML private var hexagon: Polygon = _

  private val NUM_ROWS: Int = 5
  private val NUM_COLUMNS: Int = 5

  private val HEXAGON_RADIUS: Double = 25

  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    drawHexGameBoard()
  }

  private def drawHexGameBoard(): Unit = {
    for (row <- 0 until NUM_ROWS) {
      for (col <- 0 until NUM_COLUMNS) {
        val hexagonCopy: Polygon = createHexagon()

        // Shift every second row to create the honeycomb pattern and add padding to the left to create a hex board
        if (row % 2 == 1) {
          hexagonCopy.setTranslateX(HEXAGON_RADIUS * Math.sqrt(3) * (col + Math.ceil(row / 2 + 0.5)))
        } else {
          hexagonCopy.setTranslateX(HEXAGON_RADIUS * Math.sqrt(3) * (col + Math.floor(row / 2)) + HEXAGON_RADIUS * Math.sqrt(3) / 2)
        }

        hexagonCopy.setTranslateY(HEXAGON_RADIUS * 3 / 2 * row) // Adjust the y-coordinate

        gameBoard.getChildren.add(hexagonCopy) // Add the hexagon to the game board
      }
    }
  }

  private def createHexagon(): Polygon = {
    val newHexagon: Polygon = new Polygon
    newHexagon.getPoints.addAll(
      0.0, HEXAGON_RADIUS,
      HEXAGON_RADIUS * Math.sqrt(3) / 2, HEXAGON_RADIUS / 2,
      HEXAGON_RADIUS * Math.sqrt(3) / 2, -HEXAGON_RADIUS / 2,
      0.0, -HEXAGON_RADIUS,
      -HEXAGON_RADIUS * Math.sqrt(3) / 2, -HEXAGON_RADIUS / 2,
      -HEXAGON_RADIUS * Math.sqrt(3) / 2, HEXAGON_RADIUS / 2
    )
    newHexagon.setFill(Color.WHITE) // Set the hexagon fill color
    newHexagon.setStroke(Color.BLACK) // Set the hexagon stroke color
    newHexagon.setStrokeWidth(1) // Set the hexagon stroke width
    newHexagon
  }
}