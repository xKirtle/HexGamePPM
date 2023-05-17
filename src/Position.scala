case class Position(x: Int, y: Int) {
  def bottomRight: Position = copy(y = this.y + 1)
  def bottomLeft: Position = Position(this.x + 1, this.y - 1)
  def topRight: Position = Position(this.x - 1, this.y + 1)
  def right: Position = copy(x = this.x + 1)
  def left: Position = copy(x = this.x - 1)
}