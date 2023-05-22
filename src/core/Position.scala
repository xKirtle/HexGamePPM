package core

case class Position(x: Int, y: Int) {
  def bottomRight: Position = copy(y = this.y + 1)

  def bottomLeft: Position = Position(this.x + 1, this.y - 1)

  def topRight: Position = Position(this.x - 1, this.y + 1)

  def right: Position = copy(x = this.x + 1)

  def left: Position = copy(x = this.x - 1)
}

object Position {
  def positionToIndex(position: Position, maxWidth: Int): Int = maxWidth * position.x + position.y

  def indexToPosition(index: Int, maxWidth: Int): Position = Position(index / maxWidth, index % maxWidth)
  
  def zero = Position(0, 0)
}
