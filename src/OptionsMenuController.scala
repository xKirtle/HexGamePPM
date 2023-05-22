// OptionsMenuController.scala

import OptionsMenuController.loadFXMLWithArgs
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Node, Parent, Scene}
import javafx.stage.Stage
import javafx.scene.control.Button
import javafx.event.ActionEvent

class OptionsMenuController {
  @FXML private var vsPlayerButton: Button = _
  @FXML private var vsCPUButton: Button = _

  @FXML
  def versusPlayer(event: ActionEvent): Unit = {
    loadFXMLWithArgs("Game.fxml", event.getSource, loader => {
      val controller = new GameController
      controller.isOpponentCPU = false
      loader.setController(controller)
      loader
    })
  }

  @FXML
  def versusCPU(event: ActionEvent): Unit = {
    loadFXMLWithArgs("Game.fxml", event.getSource, loader => {
      val controller = new GameController
      controller.isOpponentCPU = true
      loader.setController(controller)
      loader
    })
  }
}

object OptionsMenuController {
  def loadFXMLWithArgs(fxmlFile: String, eventSource: AnyRef, loaderFunction: FXMLLoader => FXMLLoader): Unit = {
    val loader: FXMLLoader = loaderFunction(new FXMLLoader(getClass.getResource(fxmlFile)))
    val root: Parent = loader.load()
    val stage: Stage = eventSource.asInstanceOf[Node].getScene.getWindow.asInstanceOf[Stage]
    val scene: Scene = new Scene(root)
    stage.setScene(scene)
    stage.show()
  }
}