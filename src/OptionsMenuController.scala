// OptionsMenuController.scala

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
    loadFXMLWithArgs("Game.fxml", event, isOpponentCPU = false)
  }

  @FXML
  def versusCPU(event: ActionEvent): Unit = {
    loadFXMLWithArgs("Game.fxml", event, isOpponentCPU = true)
  }
  
  def loadFXMLWithArgs(fxmlFile: String, event: ActionEvent, isOpponentCPU: Boolean): Unit = {
    val loader = new FXMLLoader(getClass.getResource(fxmlFile))
    val controller = new GameController()
    controller.isOpponentCPU = isOpponentCPU
    loader.setController(controller)

    val root: Parent = loader.load()
    val stage: Stage = event.getSource.asInstanceOf[Node].getScene.getWindow.asInstanceOf[Stage]
    val scene: Scene = new Scene(root)
    stage.setScene(scene)
    stage.show()
  }
}