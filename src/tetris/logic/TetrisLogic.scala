package tetris.logic
import java.io.PrintWriter
import scala.io.{BufferedSource, Source}
import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.game.TetrisGame
import tetris.logic.TetrisLogic._
import java.io._
//import scala.sys.process.processInternal.File

class TetrisLogic(val randomGen: RandomGenerator,
                  val gridDims : Dimensions,
                  val initialBoard: Seq[Seq[CellType]]) {

  def this(random: RandomGenerator, gridDims : Dimensions) = {
    this(random, gridDims, makeEmptyBoard(gridDims))
  }
  val saveFile = "data/saveFile/data.txt"
  var wallKick : Boolean = true // wall kick happens once per tetromino


  def readSaveFile : GameData = {
    val file: BufferedSource = scala.io.Source.fromFile(saveFile)
    val data = file.getLines().toArray
    val highScore = data(0).toInt
    file.close()
    GameData(0 , highScore , 1  , 0)
  }
  def initializeRightBoard() : Seq[Seq[CellType]] = {
    val arr : Array[Array[CellType]] = Array.fill(height)(Array.fill(gridDims.width  - width)(Border))
    for(i <- 0 until height ; j <- 0 until gridDims.width - width ) {
      if ((i > 1 && i < 5) || (i > 7 && i < 10) || (i > 11 && i < 14) || (i > 15 && i < 18)) {
        if (j > 1 && j != 7) {
          arr(i)(j) = Empty
        }
      }
    }
    arr.map(_.toSeq)
  }
  def showNextPiece() : Unit = {
    val midOfBox = Point(3 , 4)
    for(i <- 0 until 3 ; j <- 0 until 4) {
      frame = frame.copy(rightBoard = frame.rightBoard.updated(2 + i, frame.rightBoard(2 + i).updated(2 + j , Empty)))
      val toShow = frame.next.getCopy(midOfBox)
      (0 until 4).foreach(i => frame = frame.copy(rightBoard = updateBoard(toShow.position(i) , toShow.cellType , frame.rightBoard)))
    }
}
  def showShadow() : Unit = {
    def removeShadow(cellType: CellType) : CellType = if (isShadow(cellType)) Empty else cellType
    frame = frame.copy(board = frame.board.map(_.map(removeShadow))
    )
    var c = frame.activePiece.getCopy(frame.activePiece.anchor)
    while(canMove(c.moveDown.position , frame.board)) c = c.moveDown
    c = getShadowColor(c)
    (0 until 4).foreach(i => frame = frame.copy(board = updateBoard(c.position(i) , c.cellType , frame.board)))

  }
  def getShadowColor(t : Tetromino) : Tetromino = {
    t.cellType match {
      case ICell => t.getCopy(ShadowI)
      case SCell => t.getCopy(ShadowS)
      case LCell => t.getCopy(ShadowL)
      case ZCell => t.getCopy(ShadowZ)
      case TCell => t.getCopy(ShadowT)
      case OCell => t.getCopy(ShadowO)
      case JCell => t.getCopy(ShadowJ)
    }
  }
  def isPiece(c: CellType) : Boolean = c == ICell || c == JCell || c == OCell || c == TCell || c == ZCell || c == LCell || c ==SCell
  def isShadow(c : CellType) : Boolean = c == ShadowI || c == ShadowJ || c == ShadowO || c == ShadowT || c ==  ShadowZ || c == ShadowL || c == ShadowS
  val width = gridDims.width - 8
  val height = gridDims.height
  def this() = this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))
  var isHold : Boolean = false
  var frame : Frame = Frame(getNextPiece , getNextPiece , initialBoard ,
    initializeRightBoard()
    ,lost = false)
  var gameData : GameData = readSaveFile
  showNextPiece()
  showShadow()

  def getCellType(p : Point): CellType = {
    if (frame.activePiece.position.contains(p)) frame.activePiece.cellType
    else if (p.x < width)  frame.board(p.y)(p.x)
    else frame.rightBoard(p.y)(p.x - width)
  }
  def isGameOver: Boolean = frame.lost
  def overlap(p : Point , s : Seq[Seq[CellType]]): Boolean = isPiece(s(p.y)(p.x))

  def canMove(piece : Vector[Point] , pieces : Seq[Seq[CellType]]) : Boolean = {
    def outOfBound(p : Point) : Boolean = p.x > width - 1 || p.x < 0 || p.y > height - 1 || p.y < 0
    piece.forall(!outOfBound(_)) && piece.forall(!overlap(_ , pieces))
  }

  def rotateLeft(): Unit = {

    if(canMove(frame.activePiece.rotateLeft.position , frame.board)) {
      frame = frame.copy(activePiece = frame.activePiece.rotateLeft)
//      showShadow()
    }
    else if (wallKick){
      wallKick = false
      val p = frame.activePiece
      if (canMove(p.moveRight.rotateLeft.position , frame.board)) frame = frame.copy(activePiece = frame.activePiece.moveRight.rotateLeft)    //try to move right then rotate
      if (p.position == frame.activePiece.position) {                                                                                         //if earlier try doesn't succeed
        if (canMove(p.moveLeft.rotateLeft.position , frame.board) )frame = frame.copy(activePiece = frame.activePiece.moveLeft.rotateLeft)    //try to move left the rotate
      }
      if (p.position == frame.activePiece.position) wallKick = true        //if both don't succeed then the piece didn't wall kick and should be able to do so at a later point
    }
    showShadow()
  }

  def rotateRight(): Unit = {
    if(canMove(frame.activePiece.rotateRight.position , frame.board)) {
      frame = frame.copy(activePiece = frame.activePiece.rotateRight)
    }
    else if (wallKick){
      wallKick = false
      val p = frame.activePiece
      if (canMove(p.moveRight.rotateRight.position , frame.board)) frame = frame.copy(activePiece = frame.activePiece.moveRight.rotateRight)
      if (p.position == frame.activePiece.position) {
        if (canMove(p.moveLeft.rotateRight.position , frame.board) )frame = frame.copy(activePiece = frame.activePiece.moveLeft.rotateRight)
      }
      if (p.position == frame.activePiece.position) wallKick = true
    }
    showShadow()
  }

  def moveLeft(): Unit = {
    if (canMove(frame.activePiece.moveLeft.position , frame.board))
      frame = frame.copy(activePiece = frame.activePiece.moveLeft)
    showShadow()
  }

  def moveRight(): Unit = {
    if (canMove(frame.activePiece.moveRight.position , frame.board))
      frame = frame.copy(activePiece = frame.activePiece.moveRight)
    showShadow()
  }

  def moveDown(): Unit  = {
    if (isHold) return

    frame = frame.copy(board = checkIfClear(frame.board))
    if (!canMove(frame.activePiece.moveDown.position , frame.board)) handlePieceArrived()
    else frame = frame.copy(activePiece = frame.activePiece.moveDown)
    showShadow()
  }
  def hold(h : Boolean) : Unit = isHold = h

  def checkIfClear(s : Seq[Seq[CellType]]) : Seq[Seq[CellType]] = {
    def getFilteredRows : Seq[Seq[CellType]] = s.filter(_.exists(x => x == Empty || isShadow(x)))
    def getNewRows(newRows : Int) : Seq[Seq[CellType]] = Seq.fill[Seq[CellType]](newRows)(Seq.fill[CellType](width)(Empty))
    updateScore(getNewRows(height - getFilteredRows.length).length)
    getNewRows(height - getFilteredRows.length) ++ getFilteredRows
  }
  def updateScore(n : Int) : Unit = {
    n match{
      case 0 =>
      case 1 => gameData = gameData.copy(score = gameData.score + gameData.level * 100)
      case 2 => gameData = gameData.copy(score = gameData.score + gameData.level * 300)
      case 3 => gameData = gameData.copy(score = gameData.score + gameData.level * 500)
      case 4 => gameData = gameData.copy(score = gameData.score + gameData.level * 800)
    }
      gameData = gameData.copy(lines = gameData.lines + n , level = gameData.score / 500 + 1)
    if(gameData.highScore < gameData.score) gameData = gameData.copy(highScore = gameData.score)
  }

  def doHardDrop(): Unit = {
    if (isHold) return
    var steps = 0
    while(canMove(frame.activePiece.moveDown.position , frame.board)) {
      moveDown()
      steps += 1
    }
    gameData = gameData.copy(score = gameData.score + steps)
    if(gameData.highScore < gameData.score) gameData = gameData.copy(highScore = gameData.score)
    handlePieceArrived()
  }

  def updateBoard(p : Point , t : CellType , s : Seq[Seq[CellType]]) : Seq[Seq[CellType]] = s.updated(p.y  , s(p.y).updated(p.x , t))
  def handlePieceArrived() : Unit = {

    if(isGameOver) return
    (0 until 4).foreach(i => frame = frame.copy(board = updateBoard(frame.activePiece.position(i) , frame.activePiece.cellType , frame.board)))   //adds the piece that's just dropped to the board
    frame = frame.copy(board = checkIfClear(frame.board))
    frame = frame.copy(activePiece = frame.next , next = getNextPiece)
    wallKick = true
    showNextPiece()
    if (frame.activePiece.position.exists(overlap(_ , frame.board))) {

      frame = frame.copy(lost = true)
      val pw = new PrintWriter(new File(saveFile))
      pw.write(gameData.highScore.toString)
      pw.close
    }
    showShadow()
  }

  def getNextPiece : Tetromino = {
    val startingPoint : Point = Point(
      if (width % 2 == 0) width /  2  - 1 else width / 2 ,
      1
    )
    randomGen.randomInt(upTo = 7) match{
      case 0 => rotationTypeTwo(startingPoint , Vector(Point(-1,0),Point(1,0),Point(0,0),Point(2,0)) , ICell)
      case 1 => rotationTypeOne(startingPoint , Vector(Point(-1,-1),Point(-1,0),Point(0,0),Point(1,0)) , JCell)
      case 2 => rotationTypeOne(startingPoint , Vector(Point(-1,0),Point(1,0),Point(0,0),Point(1,-1)) , LCell)
      case 3 => rotationTypeThree(startingPoint , Vector(Point(0,-1),Point(1,0),Point(0,0),Point(1,-1)) , OCell)
      case 4 => rotationTypeOne(startingPoint , Vector(Point(-1,0),Point(1,-1),Point(0,0),Point(0,-1)) , SCell)
      case 5 => rotationTypeOne(startingPoint , Vector(Point(-1,0),Point(0,-1),Point(0,0),Point(1,0)) , TCell)
      case 6 => rotationTypeOne(startingPoint , Vector(Point(-1,-1),Point(0,-1),Point(0,0),Point(1,0)) , ZCell)
    }
  }
  case class Frame(
                    activePiece : Tetromino,
                    next : Tetromino,
                    board : Seq[Seq[CellType]] ,
                    rightBoard : Seq[Seq[CellType]],
                    lost : Boolean
                  )
  case class GameData(
                  score : Int,
                  highScore : Int,
                  level : Int,
                  lines : Int
                  )
}
object TetrisLogic {

  val FramesPerSecond: Int = 2 // change this to speed up or slow down the game
  val DrawSizeFactor = 2.0 // increase this to make the game bigger (for high-res screens)

  def makeEmptyBoard(gridDims : Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width - 8)(Empty)

    Seq.fill(gridDims.height)(emptyLine)



  }
  val DefaultWidth: Int = 18
  val NrTopInvisibleLines: Int = 0
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims : Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)

  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))
}
