import javax.swing.{JFrame, JPanel}
import java.awt.Color
import java.awt.Graphics

object Display {
  val PIXEL_SCALE = 8
  val BACKGROUND_COLOR = Color.BLACK
}

class Display(private val memory: Memory) {
  val window: JFrame = new JFrame
  window.setSize(
    Memory.VIDEO_WIDTH * Display.PIXEL_SCALE,
    (Memory.VIDEO_HEIGHT * Display.PIXEL_SCALE) + 30)
  window.setContentPane(new FrameBufferPanel(memory))
  window.setVisible(true)
  window.setBackground(Display.BACKGROUND_COLOR)

  def update(): Unit = {
    window.repaint()
  }
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
