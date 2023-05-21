import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class HelloWorld extends Application {

  override def start(primaryStage: Stage): Unit = {
    val fxmlLoader = new FXMLLoader(getClass.getResource("/Game.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}

object HelloWorld {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[HelloWorld], args: _*)
  }
}
