/*
  SuperChip Emulator.

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

class Emulator(private val memory: Memory) {
  def start(): Unit = {
    val controller = new Controller
    val display = new Display(memory, controller)
    val cpu = new Cpu(memory, controller)

    var lastDelayDecrement = System.currentTimeMillis()
    val delayThread = new Thread {
      override def run(): Unit = {
        while(!isInterrupted) {
          val elapsed = System.currentTimeMillis() - lastDelayDecrement
          // Roughly 60 Hz
          if (elapsed >= 16) {
            cpu.decrementDelay
            lastDelayDecrement = System.currentTimeMillis()
          }
        }
      }
    }

    delayThread.start()
    display.start()

    while(display.isRunning()) {
      val instruction = cpu.fetch
      cpu.execute(instruction)
      val sleepMs:Double = 1000/EmulatorParameters.CPU_FREQUENCY_HZ
      val sleepNano = (sleepMs * 1000).toInt
      Thread.sleep(0, sleepNano)
    }

    delayThread.interrupt()
    delayThread.join()
  }
}
