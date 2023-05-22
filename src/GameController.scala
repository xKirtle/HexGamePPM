import GameController.createHexagon
import OptionsMenuController.loadFXMLWithArgs
import core.Position.{indexToPosition, positionToIndex}
import core.{Cells, GameState, MoveGenerator, Position}
import javafx.fxml.FXML
import javafx.fxml.Initializable
import javafx.scene.input.MouseEvent
import javafx.scene.layout.GridPane
import javafx.scene.{Group, Node}
import javafx.scene.paint.Color
import javafx.scene.shape.{LineTo, MoveTo, Path, Polygon}
import javafx.scene.text.Text

import java.net.URL
import java.util.ResourceBundle
import scala.annotation.tailrec

class GameController extends Initializable {
  @FXML private var gameBoard: GridPane = _
  @FXML private var rootGroup: Group = _
  @FXML private var currentPlayerText: Text = _
  
  var isOpponentCPU: Boolean = false
  var difficulty: Int = 0 // 0 - easy, 1 - medium, 2 - hard

  private val boardSize: Int = 5
  private val numRows: Int = boardSize
  private val numColumns: Int = boardSize
  private val hexagonRadius: Double = 25
  
  private var gameOver: Boolean = false
  private var gameState: GameState = GameState.createNewGameState(boardSize, Cells.Red)
  private var moveGenerator: MoveGenerator = MoveGenerator()
  
  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    drawHexGameBoard()
    currentPlayerText.setText(s"Current player: ${gameState.currentPlayer}")
  }
  
  @FXML
  private def onUndoClicked(): Unit = {
    val amount = if (isOpponentCPU) 2 else 1

    gameState = gameState.undoPlay(amount)
    updateHexagonsWithBoardValues()
    
    if (gameOver) {
      val winningLine = rootGroup.getChildren.stream()
        .filter(node => node.isInstanceOf[Path] && node.getId == "winningLine")
        .findFirst
        .orElse(null)

      // Should never be null..
      if (winningLine != null) {
        rootGroup.getChildren.remove(winningLine)
      }
      
      gameOver = false
    }

    currentPlayerText.setText(s"Current player: ${gameState.currentPlayer}")
  }

  @FXML
  private def handleHexagonClick(hexagon: Polygon): Unit = {
    // Position already played or game over
    if (hexagon.getFill != Color.WHITE || gameOver) return

    val position = indexToPosition(hexagon.getId.toInt, numRows)
    val player = gameState.currentPlayer
    makeMove(position, player, if (player == Cells.Red) Color.RED else Color.BLUE)

    // Handle CPU turn
    if (isOpponentCPU && !gameOver) {
      val cpuPlayer = gameState.currentPlayer
      
      val (randomMove, newMoveGenerator) = difficulty match {
        case 0 => moveGenerator.randomMove(gameState.board)
        case 1 => moveGenerator.betterRandomMove(gameState)
        case 2 => moveGenerator.weightedMove(gameState)
      }
      
      moveGenerator = newMoveGenerator
      makeMove(randomMove, cpuPlayer, if (cpuPlayer == Cells.Red) Color.RED else Color.BLUE)
    }
  }
  
  @FXML
  private def onGoBackClicked(event: MouseEvent): Unit = {
    loadFXMLWithArgs("OptionsMenu.fxml", "Options Menu", event.getSource, loader => loader)
  }
  
  private def drawHexGameBoard(): Unit = {
    @tailrec
    def drawRow(position: Position): Unit = {
      if (position.x < numRows) {
        drawColumn(position.copy(y = 0))
        drawRow(position.copy(x = position.x + 1))
      }
    }

    @tailrec
    def drawColumn(position: Position): Unit = {
      val Position(x, y) = position
      if (y < numColumns) {
        val hexagonCopy: Polygon = createHexagon(hexagonRadius)
        hexagonCopy.setId(positionToIndex(position, numRows).toString)

        // Shift every second row to create the honeycomb pattern and add padding to the left to create a hex board
        if (x % 2 == 1) {
          hexagonCopy.setTranslateX(hexagonRadius * Math.sqrt(3) * (y + Math.ceil(x / 2 + 0.5)))
        } else {
          hexagonCopy.setTranslateX(hexagonRadius * Math.sqrt(3) * (y + Math.floor(x / 2)) + hexagonRadius * Math.sqrt(3) / 2)
        }

        hexagonCopy.setTranslateY(hexagonRadius * 3 / 2 * x)
        hexagonCopy.setOnMouseClicked((_: MouseEvent) => {
          handleHexagonClick(hexagonCopy)
        })

        gameBoard.getChildren.add(hexagonCopy)

        drawColumn(position.copy(y = y + 1))
      }
    }

    drawRow(Position.zero)
  }
  
  private def tryToDisplayWinningPathForPlayer(player: Cells.Cell): Unit = {
    val winningPath: List[Position] = GameState.getContiguousLineDFS(player, gameState.board)
    
    if (winningPath.nonEmpty) {
      val winningLine = new Path()
      winningLine.setId("winningLine")
      displayWinningPath(winningPath, 0, winningLine)
      
      winningLine.setStroke(Color.YELLOW)
      winningLine.setStrokeWidth(3)
      
      rootGroup.getChildren.add(winningLine)
      gameOver = true
    }

    @tailrec
    def displayWinningPath(winningPath: List[Position], i: Int, winningLine: Path): Unit = {
      if (i < winningPath.length) {
        val index: Int = positionToIndex(winningPath(i), numRows)

        val node: Node = gameBoard.getChildren.get(index)
        node match {
          case winningHexagon: Polygon =>
            val centerX = winningHexagon.getBoundsInParent.getCenterX + winningHexagon.getParent.getLayoutX
            val centerY = winningHexagon.getBoundsInParent.getCenterY + winningHexagon.getParent.getLayoutY

            // Create a new LineTo path element for each hexagon in the winningPath
            if (i == 0) {
              // For the first hexagon, we need to move to the center instead of drawing a line to it
              winningLine.getElements.add(new MoveTo(centerX, centerY))
            } else {
              winningLine.getElements.add(new LineTo(centerX, centerY))
            }

            displayWinningPath(winningPath, i + 1, winningLine)
          case _ =>
        }
      }
    }
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

  private def makeMove(position: Position, player: Cells.Cell, color: Color): Unit = {
    val index = positionToIndex(position, numRows)
    gameBoard.getChildren.get(index) match {
      case hexagon: Polygon => hexagon.setFill(color)
      case _ =>
    }
    
    gameState = gameState.play(position)
    tryToDisplayWinningPathForPlayer(player)

    currentPlayerText.setText(s"Current player: ${gameState.currentPlayer}")
  }
}

object GameController {
   def createHexagon(hexagonRadius: Double): Polygon = {
    val newHexagon: Polygon = new Polygon
    newHexagon.getPoints.addAll(
      0.0, hexagonRadius,
      hexagonRadius * Math.sqrt(3) / 2, hexagonRadius / 2,
      hexagonRadius * Math.sqrt(3) / 2, -hexagonRadius / 2,
      0.0, -hexagonRadius,
      -hexagonRadius * Math.sqrt(3) / 2, -hexagonRadius / 2,
      -hexagonRadius * Math.sqrt(3) / 2, hexagonRadius / 2
    )
    newHexagon.setFill(Color.WHITE)
    newHexagon.setStroke(Color.BLACK)
    newHexagon.setStrokeWidth(1)
    newHexagon
  }
}