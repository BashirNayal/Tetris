// DO NOT MODIFY FOR BASIC SUBMISSION
// scalastyle:off

package tetris.game

import java.awt.event
import java.awt.event.KeyEvent._

import engine.{GameBase, graphics}
import engine.graphics.{Color, Point, Rectangle, Triangle}
import processing.core.{PApplet, PConstants, PFont}
import processing.event.KeyEvent
import tetris.logic._
import tetris.game.TetrisGame._
import tetris.logic.{Point => GridPoint}

class TetrisGame extends GameBase {
  var gameLogic : TetrisLogic = TetrisLogic()
  var gameLogic2 : TetrisLogic = TetrisLogic()
  val updateTimer = new UpdateTimer(TetrisLogic.FramesPerSecond.toFloat)
  var gridDims : Dimensions = gameLogic.gridDims
  var widthInPixels: Int = (WidthCellInPixels * gridDims.width).ceil.toInt
  var heightInPixels: Int = (HeightCellInPixels * gridDims.height).ceil.toInt
  var screenArea: Rectangle = Rectangle(Point(0, 0), widthInPixels.toFloat, heightInPixels.toFloat)
  var twoPlayersMode : Boolean = false
  var started = false




  override def draw(): Unit = {
    if (!started) {
      startScreen()
      updateTimer.init()
      return
    }
    drawTexts()
    if (playerOneLost() || playerTwoLost()) {
      drawGameOverScreen()
      if(!twoPlayersMode && playerOneLost()) {
        restartScreen()
        return
      }
      if(twoPlayersMode && playersLost()) {
        drawWinner()
        restartScreen()
      }
    }
  }

  def drawTexts() : Unit = {
    updateState()
    drawGrid()
    textSize(25)
    drawLevel()
    drawScore()
    drawLinesCount()
    if(twoPlayersMode)drawDifference() else drawHighScore()

  }
  def playerOneLost() : Boolean = gameLogic.isGameOver
  def playerTwoLost() : Boolean = gameLogic2.isGameOver
  def playersLost() : Boolean = playerOneLost() && playerTwoLost()

  def drawWinner() : Unit = {
    val winner = if(gameLogic2.gameData.score > gameLogic.gameData.score) "Player 2" else "Player 1"
    val area = getCell(GridPoint(gridDims.width / 2 - 10, 0))
    setFillColor(Color.Blue)
    drawText(winner + " WON!!!" + gameLogic.gameData.level, area.rightDown)
  }
  def drawLevel() : Unit = {
    fill(125,125,125)
    val area = getCell(GridPoint(0,0))
    drawText("Level: " + gameLogic.gameData.level, area.rightDown)

    if (!twoPlayersMode) return
    val area2 = getCell(GridPoint( gridDims.width / 2 , 0))
    drawText("Level: " + gameLogic2.gameData.level, area2.rightDown)

  }
  def drawDifference() : Unit = {
    val score1 = gameLogic.gameData.score
    val score2 = gameLogic2.gameData.score
    val area1 = getCell(GridPoint(11,17))
    val area2 = getCell(GridPoint(11 + gridDims.width / 2,17))

    drawText("Diff: " + (score1 - score2).toString, area1.rightUp)
    drawText("Diff: " + (score2 - score1).toString, area2.rightUp)
  }

  def drawScore() : Unit = {
    fill(255,255,255)
    val area = getCell(GridPoint(11,13))
    drawText("SCORE: " + gameLogic.gameData.score, area.rightUp)
    if (!twoPlayersMode) return

    val area2 = getCell(GridPoint(11 + gridDims.width / 2 ,13))
    drawText("SCORE: " + gameLogic2.gameData.score, area2.rightUp)
  }

  def drawHighScore() : Unit = {
    fill(255,255,255)
    val area = getCell(GridPoint(11,17))
    drawText("BEST: " + gameLogic.gameData.highScore, area.rightUp)
  }
  def drawLinesCount() : Unit = {
    fill(255,255,255)
    val area = getCell(GridPoint(11,9))
    drawText("LINES: " + gameLogic.gameData.lines, area.rightUp)
    if (!twoPlayersMode) return

    val area2 = getCell(GridPoint(11 + gridDims.width / 2 ,9))
    drawText("LINES: " + gameLogic2.gameData.lines, area2.rightUp)
  }

  def drawGameOverScreen(): Unit = {
    if(playerOneLost()) {
      val area = getCell(GridPoint(9, 10))
      setFillColor(Color.Red)
      drawTextCentered("GAME OVER!", 50, area.center)
    }

    if (!twoPlayersMode || !playerTwoLost()) return
    val area2 = getCell(GridPoint(9 + gridDims.width / 2 , 10))
    setFillColor(Color.Red)
    drawTextCentered("GAME OVER!", 50, area2.center)

  }

  def getCell(p : GridPoint): Rectangle = {
    val widthPerCell = screenArea.width / gridDims.width
    val heightPerCell = screenArea.height / gridDims.height
    val leftUp = Point(screenArea.left + p.x * widthPerCell,
      screenArea.top + p.y * heightPerCell)
    Rectangle(leftUp, widthPerCell, heightPerCell)
  }

  def drawGrid(): Unit = {
    for (p <- gridDims.allPointsInside) {
      if(!twoPlayersMode ||  p.x < gridDims.width / 2) drawCell(getCell(p), gameLogic.getCellType(p))
      else drawCell(getCell(GridPoint(p.x, p.y)), gameLogic2.getCellType(GridPoint(p.x - gridDims.width / 2 , p.y)))
    }


    def drawCell(area: Rectangle, tetrisColor: CellType): Unit = {
      val color = tetrisBlockToColor(tetrisColor)
      setFillColor(color)
      drawRectangle(area)
      //this skips drawing the design for the empty cells
      if(tetrisColor != Empty) drawDesign(tetrisColor , area)

      def drawDesign(c : CellType , r : Rectangle) : Unit = {
        val decorationSize : Float = (4 * TetrisLogic.DrawSizeFactor).toFloat
        noStroke()
        /*

             \_/     draws this shape inside a rectangle on its four edges. Each time it gets colored a shade of the block's color

        */
        setFillColor(tetrisBlockToColor(c).darken(0.75f))
        rect(r.leftUp.x + decorationSize, r.leftUp.y, r.width - decorationSize * 2 , decorationSize)     //top rec
        triangle(r.leftUp.x , r.leftUp.y , r.leftUp.x + decorationSize , r.leftUp.y , r.leftUp.x + decorationSize , r.leftUp.y + decorationSize)
        triangle(r.rightUp.x , r.rightUp.y , r.rightUp.x - decorationSize , r.rightUp.y , r.rightUp.x - decorationSize , r.rightUp.y + decorationSize)

        setFillColor(tetrisBlockToColor(c).darken(0.9f))
        rect(r.leftUp.x, r.leftUp.y + decorationSize, decorationSize , r.width - decorationSize * 2)     //left rec
        triangle(r.leftDown.x , r.leftDown.y , r.leftDown.x , r.leftDown.y - decorationSize, r.leftDown.x + decorationSize , r.leftDown.y - decorationSize)
        triangle(r.leftUp.x , r.leftUp.y , r.leftUp.x + decorationSize , r.leftUp.y + decorationSize, r.leftUp.x , r.leftUp.y + decorationSize)

        setFillColor(tetrisBlockToColor(c).darken(0.35f))
        rect(r.leftDown.x + decorationSize, r.leftDown.y - decorationSize, r.width - decorationSize * 2 , decorationSize)     //bottom rec
        triangle(r.leftDown.x , r.leftDown.y , r.leftDown.x + decorationSize , r.leftDown.y , r.leftDown.x + decorationSize , r.leftDown.y - decorationSize)
        triangle(r.rightDown.x , r.rightDown.y , r.rightDown.x - decorationSize , r.rightDown.y , r.rightDown.x - decorationSize , r.rightDown.y - decorationSize)

        setFillColor(tetrisBlockToColor(c).darken(0.5f))
        rect(r.rightUp.x - decorationSize, r.rightUp.y +decorationSize  , decorationSize , r.width - decorationSize * 2)     //right rec
        triangle(r.rightDown.x , r.rightDown.y , r.rightDown.x - decorationSize , r.rightDown.y - decorationSize, r.rightDown.x, r.rightDown.y - decorationSize)
        triangle(r.rightUp.x , r.rightUp.y , r.rightUp.x - decorationSize , r.rightUp.y + decorationSize , r.rightUp.x  , r.rightUp.y + decorationSize)
      }
    }
  }

  override def keyPressed(event: KeyEvent): Unit = {

    event.getKeyCode match {
      case VK_V     => gameLogic.rotateLeft()
      case VK_B     => gameLogic.rotateRight()
      case VK_W    => gameLogic.rotateRight()
      case VK_S  => gameLogic.moveDown()
      case VK_A  => gameLogic.moveLeft()
      case VK_D => gameLogic.moveRight()
      case VK_SPACE => gameLogic.doHardDrop()
      case VK_N => gameLogic.hold(true)

      case VK_K => gameLogic2.rotateLeft()
      case VK_L => gameLogic2.rotateRight()
      case VK_UP => gameLogic2.rotateRight()
      case VK_DOWN => gameLogic2.moveDown()
      case VK_LEFT => gameLogic2.moveLeft()
      case VK_RIGHT => gameLogic2.moveRight()
      case VK_ENTER => gameLogic2.doHardDrop()
      case VK_J => gameLogic2.hold(true)
      case _        => ()
    }

  }
  override def keyReleased(event: KeyEvent): Unit = {
    event.getKeyCode match {
      case VK_N => gameLogic.hold(false)
      case VK_J => gameLogic2.hold(false)
      case _    => ()
    }
  }


  override def settings(): Unit = {
    pixelDensity(displayDensity())
    size(widthInPixels, heightInPixels)

  }
  def restartScreen() : Unit = {
    val reStartBox = Rectangle(Point(gameLogic.gridDims.width / 2 + 5f, (height * 0.9).toFloat) , 220 , 50 , "Re-start")
    setFillColor(Color(100,100,100,100))
    noStroke()
    if(reStartBox.contains(Point(mouseX.toFloat , mouseY.toFloat))) setFillColor(Color(255,123,123,55))
    drawRectangle(reStartBox)
    if (mousePressed && reStartBox.contains(Point(mouseX.toFloat , mouseY.toFloat))) {
      gameLogic = TetrisLogic()
      if(twoPlayersMode) gameLogic2 = TetrisLogic()
    }
    stroke(0)
    textSize(25)
  }

  def startScreen() : Unit = {
    val im = loadImage("data/image/tetris-1.jpg")
    image(im , -300 , 0)
    val startBox = Rectangle(Point((width/2).toFloat - 50 , (height * 0.9).toFloat) , 90 , 30 , "Start")
    val onePlayer = Rectangle(Point((width/2).toFloat - 160 , (height * 0.8).toFloat) , 130 , 30 , "1 Player")
    val twoPlayers = Rectangle(Point((width/2).toFloat + 40 , (height * 0.8).toFloat) , 130 , 30 , "2 Players")
    noStroke()
    setFillColor(Color(100,100,100,100))
    if(startBox.contains(Point(mouseX.toFloat , mouseY.toFloat))) setFillColor(Color(255,123,123,30))
    drawRectangle(startBox)

    if(twoPlayersMode) setFillColor(Color(100,100,100,100)) else setFillColor(Color.shadowGreen)
    drawRectangle(onePlayer)

    if(!twoPlayersMode) setFillColor(Color(100,100,100,100)) else setFillColor(Color.shadowGreen)
    drawRectangle(twoPlayers)
    if (mousePressed && onePlayer.contains(Point(mouseX.toFloat , mouseY.toFloat))) twoPlayersMode = false
    if (mousePressed && twoPlayers.contains(Point(mouseX.toFloat , mouseY.toFloat))) twoPlayersMode = true
    if (mousePressed && startBox.contains(Point(mouseX.toFloat , mouseY.toFloat))) {
      started = true
      if(twoPlayersMode){
        gridDims  = gameLogic.gridDims + gameLogic2.gridDims
        widthInPixels = (WidthCellInPixels * gridDims.width).ceil.toInt
        heightInPixels = (HeightCellInPixels * gridDims.height).ceil.toInt
        screenArea = Rectangle(Point(0, 0), widthInPixels.toFloat, heightInPixels.toFloat)
        surface.setSize(widthInPixels, heightInPixels)
      }
    }
  }

  override def setup(): Unit = {

    val mono : PFont  = createFont("data/font/DS-DIGII.TTF", 25);
    background(0);
    textFont(mono);
    text("", 0, 0)
    updateTimer.init()
  }

  def updateState(): Unit = {
    if (updateTimer.timeForNextFrame()) {
      if(!gameLogic.isGameOver) gameLogic.moveDown()
      if (twoPlayersMode && !gameLogic2.isGameOver) gameLogic2.moveDown()
      updateTimer.advanceFrame()
    }
  }

  def tetrisBlockToColor(color: CellType): Color =
    color match {
      case ICell => Color.LightBlue
      case OCell => Color.Yellow
      case LCell => Color.Orange
      case JCell => Color.Blue
      case SCell => Color.Green
      case Empty  => Color.Black
      case TCell => Color.Purple
      case ZCell => Color.Red
      case Border => Color.Gray
      case ShadowI => Color.shadowLightBlue
      case ShadowS => Color.shadowGreen
      case ShadowL => Color.shadowOrange
      case ShadowJ => Color.shadowBlue
      case ShadowZ => Color.shadowRed
      case ShadowO => Color.shadowYellow
      case ShadowT => Color.shadowPurple
      case WhiteCell => Color.test

    }
}

object TetrisGame {

  val WidthCellInPixels: Double = 15 * TetrisLogic.DrawSizeFactor
  val HeightCellInPixels: Double = WidthCellInPixels

  def main(args:Array[String]): Unit = {
    PApplet.main("tetris.game.TetrisGame")
  }

}