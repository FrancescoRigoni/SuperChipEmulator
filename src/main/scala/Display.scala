/*
  Chip8 Emulator.

  Copyright (C) 2018 Francesco Rigoni - francesco.rigoni@gmail.com
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License v3 as published by
  the Free Software Foundation.
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.
  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

import javax.swing.{JFrame, JPanel}
import java.awt.{Color, Dimension, Graphics}
import java.awt.event.{KeyEvent, KeyListener, WindowEvent, WindowListener}
import java.util.concurrent.atomic.AtomicBoolean

object Display {
  val PIXEL_SCALE = 12
  val BACKGROUND_COLOR = Color.BLACK
  val PIXEL_COLOR = new Color(50, 0, 255)
  val REFRESH_RATE_HZ = 60
  val SLEEP_TIME_BETWEEN_RENDERS_MS = 1000 / REFRESH_RATE_HZ
}

class Display(private val memory: Memory, private val controller: Controller) extends KeyListener with WindowListener {
  val window: JFrame = new JFrame
  var running: AtomicBoolean = new AtomicBoolean(true)

  private val renderThread = new Thread {
    override def run() : Unit = {
      while (true) {
        window.repaint()
        try {
          Thread.sleep(Display.SLEEP_TIME_BETWEEN_RENDERS_MS)
        } catch {
          case _:InterruptedException => {
            running.set(false)
            return
          }
        }
      }
    }
  }

  def start(): Unit = {
    val frame = new FrameBufferPanel(memory)
    window.setContentPane(frame)
    window.getContentPane.setSize(new Dimension(Memory.VIDEO_WIDTH * Display.PIXEL_SCALE, Memory.VIDEO_HEIGHT * Display.PIXEL_SCALE))
    window.getContentPane.setPreferredSize(new Dimension(Memory.VIDEO_WIDTH * Display.PIXEL_SCALE, Memory.VIDEO_HEIGHT * Display.PIXEL_SCALE))
    window.pack()
    window.setVisible(true)
    window.setTitle(EmulatorParameters.NAME)
    window.setResizable(false)
    window.addWindowListener(this)
    window.setBackground(Display.BACKGROUND_COLOR)
    window.addKeyListener(this)
  }

  def isRunning() : Boolean = {
    running.get()
  }

  override def keyPressed(e: KeyEvent): Unit = {
    controller.setKeyPressed(e.getKeyChar, true)
  }

  override def keyReleased(e: KeyEvent): Unit = {
    controller.setKeyPressed(e.getKeyChar, false)
  }

  override def keyTyped(e: KeyEvent): Unit = {}

  override def windowOpened(e: WindowEvent): Unit = {
    running.set(true)
    renderThread.start()
  }

  override def windowActivated(e: WindowEvent): Unit = {}

  override def windowClosing(e: WindowEvent): Unit = {
    renderThread.interrupt()
    renderThread.join()
  }

  override def windowDeactivated(e: WindowEvent): Unit = {}

  override def windowIconified(e: WindowEvent): Unit = {}

  override def windowClosed(e: WindowEvent): Unit = {}

  override def windowDeiconified(e: WindowEvent): Unit = {}
}

class FrameBufferPanel(private val memory:Memory) extends JPanel {
  override def paintComponent(g: Graphics): Unit = {
    for (y <- 0 until Memory.VIDEO_HEIGHT) {
      for (x <- 0 until Memory.VIDEO_WIDTH) {
        val absolutePosInVideoMemory = y * Memory.VIDEO_WIDTH + x
        if (memory.video(absolutePosInVideoMemory)) drawLightSquare(g, x, y)
        else drawDarkSquare(g, x, y)
      }
    }
  }

  def drawLightSquare(g:Graphics, x: Int, y: Int): Unit = {
    g.setColor(Display.PIXEL_COLOR)
    g.fillRect(x*Display.PIXEL_SCALE, y*Display.PIXEL_SCALE, Display.PIXEL_SCALE, Display.PIXEL_SCALE)
  }

  def drawDarkSquare(g:Graphics, x: Int, y: Int): Unit = {
    g.setColor(Display.BACKGROUND_COLOR)
    g.fillRect(x*Display.PIXEL_SCALE, y*Display.PIXEL_SCALE, Display.PIXEL_SCALE, Display.PIXEL_SCALE)
  }
}
