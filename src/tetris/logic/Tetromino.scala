package tetris.logic

abstract class Tetromino{

  def anchor : Point
  def template : Vector[Point]
  def cellType : CellType
  def position : Vector[Point] = template.map(addTwoPoints(_ , anchor))
  def rotateRight  : Tetromino = this
  def rotateLeft : Tetromino = this
  def moveRight : Tetromino = moveTo(this , Point(anchor.x + 1 , anchor.y))
  def moveLeft : Tetromino = moveTo(this , Point(anchor.x - 1 , anchor.y))
  def moveDown : Tetromino = moveTo(this , Point(anchor.x , anchor.y + 1))

  private def addTwoPoints(p1 : Point , p2 : Point) : Point = Point(p1.x + p2.x , p1.y + p2.y)
  private def moveTo(t : Tetromino , to : Point) : Tetromino = {
    t match {
      case rotationTypeOne(_,_,_) => rotationTypeOne(to, template, cellType)
      case rotationTypeTwo(_,_,_) => rotationTypeTwo(to, template, cellType)
      case rotationTypeThree(_,_,_) => rotationTypeThree(to, template, cellType)
    }
  }
  def getCopy(newAnchor : Point) : Tetromino = {
    this match {
      case rotationTypeOne(_,_,_) => rotationTypeOne(newAnchor, template, cellType)
      case rotationTypeTwo(_,_,_) => rotationTypeTwo(newAnchor, template, cellType)
      case rotationTypeThree(_,_,_) => rotationTypeThree(newAnchor, template, cellType)
    }
  }

  def getCopy(newType: CellType) : Tetromino = {
    this match {
      case rotationTypeOne(_,_,_) => rotationTypeOne(anchor, template, newType)
      case rotationTypeTwo(_,_,_) => rotationTypeTwo(anchor, template, newType)
      case rotationTypeThree(_,_,_) => rotationTypeThree(anchor, template, newType)
    }
  }
}
case class rotationTypeOne(anchor : Point , template : Vector[Point] , cellType: CellType) extends Tetromino {

  override def rotateRight: Tetromino = this.copy(template = template.map(rightRotation))
  override def rotateLeft: Tetromino = this.copy(template = template.map(leftRotation))
  def rightRotation(p : Point) : Point = Point(-p.y , p.x)
  def leftRotation(p : Point) : Point = Point(p.y , -p.x)
}
case class rotationTypeTwo(anchor : Point , template : Vector[Point] , cellType: CellType) extends Tetromino {

  override def rotateRight: Tetromino = this.copy(template = template.map(rightRotation))
  override def rotateLeft: Tetromino = this.copy(template = template.map(leftRotation))
  def rightRotation(p: Point): Point = Point(-p.y + 1, p.x)
  def leftRotation(p: Point): Point = Point(p.y - 1 , -p.x)
}
case class rotationTypeThree(anchor: Point , template : Vector[Point] , cellType: CellType) extends Tetromino
