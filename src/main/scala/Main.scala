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

import scala.language.implicitConversions

object Implicits {
  implicit def intToShort(i: Int): Short = {
    val highByte = i & 0xFF00
    val lowByte = i & 0xFF
    (highByte | lowByte).toShort
  }

  implicit def intToByte(i: Int): Byte = {
    (i & 0xFF).toByte
  }

  implicit def shortToByte(i: Short): Byte = {
    (i & 0xFF).toByte
  }

  implicit def shortToInt(i: Short): Int = {
    i & 0xFFFF
  }

  implicit def byteToShort(i: Byte): Short = {
    i & 0xFF
  }

  implicit def byteToInt(i: Byte): Int = {
    i.toInt & 0x00FF
  }

  implicit def bytesToInts(p: (Byte, Byte)): (Int, Int) = {
    (p._1.toInt & 0x00FF, p._2.toInt & 0x00FF)
  }

  implicit def bytesToInts(p: (Byte, Byte, Byte)): (Int, Int, Int) = {
    (p._1.toInt & 0x00FF, p._2.toInt & 0x00FF, p._3.toInt & 0x00FF)
  }
}

object EmulatorParameters {
  val CPU_FREQUENCY_HZ = 1000.00
  val NAME = "SuperCHIP Emulator"
  val DEBUG_CPU = false
}

object Main extends App {
  def printHelp(): Unit = {
    println("+++ " + EmulatorParameters.NAME + " +++")
    println("")
    println("Please provide a rom path")
    println("")
  }

  var programName:String = _
  if (args.length >= 1) {
    val fileNameBuilder = new StringBuilder()
    for (a <- args) fileNameBuilder.append(a).append(" ")
    programName = fileNameBuilder.toString().substring(0, fileNameBuilder.length-1)

  } else {
    printHelp()
    System.exit(1)
  }

  println("+++ " + EmulatorParameters.NAME + " started +++")
  val memory = new Memory

  println("Loading interpreter")
  val interpreter = new Interpreter(memory)
  interpreter.loadSelf()

  println("Loading '" + programName + "'")
  val programSize = interpreter
    .loadProgram(programName)
    .getOrElse {
      System.exit(1)
      0
  }

//  val disasm = new Disassembler(memory, "disassemble.txt")
//  disasm.disassemble()
//  System.exit(0)

  println("Loaded " + programName + " : " + programSize + " bytes" )
  new Emulator(memory).start()

  println("Finished")
  System.exit(0)
}