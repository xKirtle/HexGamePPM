// OptionsMenuController.scala

import OptionsMenuController.loadFXMLWithArgs
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Node, Parent, Scene}
import javafx.stage.Stage
import javafx.scene.control.Button
import javafx.event.ActionEvent

import scala.annotation.unused

class OptionsMenuController {
  @FXML @unused private var vsPlayerButton: Button = _
  @FXML @unused private var vsCPUButtonEasy: Button = _
  @FXML @unused private var vsCPUButtonMedium: Button = _
  @FXML @unused private var vsCPUButtonHard: Button = _

  @FXML
  def versusPlayer(event: ActionEvent): Unit = {
    loadFXMLWithArgs("Game.fxml", "Hex Game vs Player", event.getSource, loader => {
      val controller = new GameController
      controller.isOpponentCPU = false
      loader.setController(controller)
      loader
    })
  }

  @FXML
  def versusCPUEasy(event: ActionEvent): Unit = {
    loadFXMLWithArgs("Game.fxml", "Hex Game vs CPU", event.getSource, loader => {
      val controller = new GameController
      controller.isOpponentCPU = true
      controller.difficulty = 0
      loader.setController(controller)
      loader
    })
  }

  @FXML
  def versusCPUMedium(event: ActionEvent): Unit = {
    loadFXMLWithArgs("Game.fxml", "Hex Game vs CPU", event.getSource, loader => {
      val controller = new GameController
      controller.isOpponentCPU = true
      controller.difficulty = 1
      loader.setController(controller)
      loader
    })
  }

  @FXML
  def versusCPUHard(event: ActionEvent): Unit = {
    loadFXMLWithArgs("Game.fxml", "Hex Game vs CPU", event.getSource, loader => {
      val controller = new GameController
      controller.isOpponentCPU = true
      controller.difficulty = 2
      loader.setController(controller)
      loader
    })
  }
  
  @FXML
  def versusCPU(event: ActionEvent): Unit = {
    loadFXMLWithArgs("Game.fxml", "Hex Game vs CPU", event.getSource, loader => {
      val controller = new GameController
      controller.isOpponentCPU = true
      loader.setController(controller)
      loader
    })
  }
}

object OptionsMenuController {
  def loadFXMLWithArgs(fxmlFile: String, windowTitle: String, eventSource: AnyRef, loaderFunction: FXMLLoader => FXMLLoader): Unit = {
    val loader: FXMLLoader = loaderFunction(new FXMLLoader(getClass.getResource(fxmlFile)))
    val root: Parent = loader.load()
    val stage: Stage = eventSource.asInstanceOf[Node].getScene.getWindow.asInstanceOf[Stage]
    val scene: Scene = new Scene(root)
    stage.setScene(scene)
    stage.setTitle(windowTitle)
    stage.show()
  }
}