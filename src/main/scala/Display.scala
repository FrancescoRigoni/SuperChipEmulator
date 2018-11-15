import javax.swing.{JFrame, JPanel}
import java.awt.Color
import java.awt.Graphics
import java.awt.event.{KeyEvent, KeyListener}

object Display {
  val PIXEL_SCALE = 12
  val BACKGROUND_COLOR = Color.BLACK
}

class Display(private val memory: Memory, private val controller: Controller) extends KeyListener {
  val window: JFrame = new JFrame
  window.setSize(
    Memory.VIDEO_WIDTH * Display.PIXEL_SCALE,
    (Memory.VIDEO_HEIGHT * Display.PIXEL_SCALE) + 30)
  window.setContentPane(new FrameBufferPanel(memory))
  window.setVisible(true)
  window.setBackground(Display.BACKGROUND_COLOR)
  window.addKeyListener(this)

  def update(): Unit = {
    window.repaint()
  }

  override def keyPressed(e: KeyEvent): Unit = {
    controller.setKeyPressed(e.getKeyChar, true)
  }

  override def keyReleased(e: KeyEvent): Unit = {
    controller.setKeyPressed(e.getKeyChar, false)
  }

  override def keyTyped(e: KeyEvent): Unit = {}
}

class FrameBufferPanel(private val memory:Memory) extends JPanel {
  override def paintComponent(g: Graphics): Unit = {
    for (y <- 0 until Memory.VIDEO_HEIGHT) {
      for (x <- 0 until Memory.VIDEO_WIDTH) {
        var absolutePosInVideoMemory = y * Memory.VIDEO_WIDTH + x
        if (memory.video(absolutePosInVideoMemory)) drawLightSquare(g, x, y)
        else drawDarkSquare(g, x, y)
      }
    }
  }

  def drawLightSquare(g:Graphics, x: Int, y: Int): Unit = {
    g.setColor(new Color(255, 0, 0))
    g.fillRect(x*Display.PIXEL_SCALE, y*Display.PIXEL_SCALE, Display.PIXEL_SCALE, Display.PIXEL_SCALE)
  }

  def drawDarkSquare(g:Graphics, x: Int, y: Int): Unit = {
    g.setColor(new Color(50, 0, 0))
    g.fillRect(x*Display.PIXEL_SCALE, y*Display.PIXEL_SCALE, Display.PIXEL_SCALE, Display.PIXEL_SCALE)
  }
}
