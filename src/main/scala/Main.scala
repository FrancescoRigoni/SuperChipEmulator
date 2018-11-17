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

import java.nio.ByteBuffer
import scala.language.implicitConversions

object Implicits {
  implicit def intToShort(i: Int): Short = {
    val highByte: Byte = (i & 0xFF00) >> 8
    val lowByte: Byte = i & 0xFF
    ByteBuffer.wrap(Array[Byte](highByte, lowByte)).getShort
  }

  implicit def intToByte(i: Int): Byte = {
    val lowByte: Byte = (i & 0xFF).toByte
    ByteBuffer.wrap(Array[Byte](0x00, lowByte)).get(1)
  }

  implicit def shortToByte(i: Short): Byte = {
    val lowByte: Byte = i & 0xFF
    ByteBuffer.wrap(Array[Byte](0x00, lowByte)).get(1)
  }

  implicit def byteToShort(i: Byte): Short = {
    ByteBuffer.wrap(Array[Byte](0x00, i)).getShort
  }
}

object EmulatorParameters {
  val CPU_FREQUENCY_HZ = 400
  val NAME = "Chip8 Emulator"
}

object Main extends App {
  def printHelp(): Unit = {
    println("+++ " + EmulatorParameters.NAME + " +++")
    println("")
    println("Please provide a rom path")
    println("")
  }

  var programName:String = _
  if (args.length == 1) {
    programName = args(0)
  } else {
    printHelp()
    System.exit(1)
  }

  println("+++ " + EmulatorParameters.NAME + " started +++")
  val memory = new Memory

  println("Loading interpreter")
  val interpreter = new Interpreter(memory)
  interpreter.loadSelf()

  println("Loading " + programName)
  val programSize = interpreter
    .loadProgram(programName)
    .getOrElse {
      System.exit(1)
      0
  }

  println("Loaded " + programName + " : " + programSize + " bytes" )
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
    cpu.execute(cpu.fetch)
    Thread.sleep(1000/EmulatorParameters.CPU_FREQUENCY_HZ)
  }

  delayThread.interrupt()
  delayThread.join()

  println("Finished")
  System.exit(0)
}