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

import java.io.{BufferedInputStream, FileInputStream, FileNotFoundException, IOException, File}

class Interpreter(val memory: Memory) {

  def loadSelf() : Unit = {
    memory.ramStartStoring(0)
    for (v <- fonts8by5) memory.ramStoreByte(v)
    memory.ramFinishStoring()

    memory.ramStartStoring(0x100)
    for (v <- fonts8by10) memory.ramStoreByte(v)
    memory.ramFinishStoring()
  }

  def loadProgram(filePath:String) : Option[Int] = {
    var in = None: Option[FileInputStream]
    var programSize: Option[Int] = None

    try {
      val file = new File(filePath)
      in = Some(new FileInputStream(file.getAbsolutePath))
      memory.ramStartStoring(Memory.PROGRAM_START)

      val bis = new BufferedInputStream(new FileInputStream(filePath))
      val buffer = Array.ofDim[Byte](1024)
      var totalRead = 0
      var read = 0
      do {
        read = bis.read(buffer, 0, 1024)
        (0 until read).iterator.foreach { v => memory.ramStoreByte(buffer(v)) }
        totalRead += read
      } while (read > 0)

      memory.ramFinishStoring()
      programSize = Option[Int](totalRead)

    } catch {
      case e: IOException => e.printStackTrace()
      case _: FileNotFoundException => {}
    } finally {
      if (in.isDefined) in.get.close()
    }

    programSize
  }

  private val fonts8by5 = Array[Byte](
    // 0
      Integer.parseInt("01100000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("01100000", 2).toByte,
    // 1
      Integer.parseInt("00100000", 2).toByte,
      Integer.parseInt("01100000", 2).toByte,
      Integer.parseInt("00100000", 2).toByte,
      Integer.parseInt("00100000", 2).toByte,
      Integer.parseInt("01110000", 2).toByte,
    // 2
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("00010000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10000000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
    // 3
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("00010000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("00010000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
    // 4
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("00010000", 2).toByte,
      Integer.parseInt("00010000", 2).toByte,
    // 5
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10000000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("00010000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
    // 6
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10000000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
    // 7
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("00010000", 2).toByte,
      Integer.parseInt("00100000", 2).toByte,
      Integer.parseInt("01000000", 2).toByte,
      Integer.parseInt("01000000", 2).toByte,
    // 8
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
    // 9
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("00010000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
    // A
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
    // B
      Integer.parseInt("11100000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("11100000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("11100000", 2).toByte,
    // C
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10000000", 2).toByte,
      Integer.parseInt("10000000", 2).toByte,
      Integer.parseInt("10000000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
    // D
      Integer.parseInt("11100000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("11100000", 2).toByte,
    // E
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10000000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10000000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
    // F
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10000000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10000000", 2).toByte,
      Integer.parseInt("10000000", 2).toByte
    )

  private val fonts8by10 = Array[Byte](
    // 0
    Integer.parseInt("00000000", 2).toByte,
    Integer.parseInt("00111100", 2).toByte,
    Integer.parseInt("01100110", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("01100110", 2).toByte,
    Integer.parseInt("00111100", 2).toByte,
    Integer.parseInt("00000000", 2).toByte,
    // 1
    Integer.parseInt("00000000", 2).toByte,
    Integer.parseInt("00011000", 2).toByte,
    Integer.parseInt("00111000", 2).toByte,
    Integer.parseInt("01111000", 2).toByte,
    Integer.parseInt("00011000", 2).toByte,
    Integer.parseInt("00011000", 2).toByte,
    Integer.parseInt("00011000", 2).toByte,
    Integer.parseInt("00011000", 2).toByte,
    Integer.parseInt("00111100", 2).toByte,
    Integer.parseInt("00000000", 2).toByte,
    // 2
    Integer.parseInt("00000000", 2).toByte,
    Integer.parseInt("01111110", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("00001100", 2).toByte,
    Integer.parseInt("00110000", 2).toByte,
    Integer.parseInt("0110000", 2).toByte,
    Integer.parseInt("11000000", 2).toByte,
    Integer.parseInt("11111110", 2).toByte,
    Integer.parseInt("00000000", 2).toByte,
    // 3
    Integer.parseInt("00000000", 2).toByte,
    Integer.parseInt("11111110", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("01111110", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("11111110", 2).toByte,
    Integer.parseInt("00000000", 2).toByte,
    // 4
    Integer.parseInt("00000000", 2).toByte,
    Integer.parseInt("00000110", 2).toByte,
    Integer.parseInt("00001110", 2).toByte,
    Integer.parseInt("00011110", 2).toByte,
    Integer.parseInt("00110110", 2).toByte,
    Integer.parseInt("01100110", 2).toByte,
    Integer.parseInt("11111111", 2).toByte,
    Integer.parseInt("00000110", 2).toByte,
    Integer.parseInt("00000110", 2).toByte,
    Integer.parseInt("00000000", 2).toByte,
    // 5
    Integer.parseInt("00000000", 2).toByte,
    Integer.parseInt("01111110", 2).toByte,
    Integer.parseInt("11000000", 2).toByte,
    Integer.parseInt("11000000", 2).toByte,
    Integer.parseInt("01111110", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("11111110", 2).toByte,
    Integer.parseInt("00000000", 2).toByte,
    // 6
    Integer.parseInt("00000000", 2).toByte,
    Integer.parseInt("01111110", 2).toByte,
    Integer.parseInt("11000000", 2).toByte,
    Integer.parseInt("11000000", 2).toByte,
    Integer.parseInt("11000000", 2).toByte,
    Integer.parseInt("11111110", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("01111110", 2).toByte,
    Integer.parseInt("00000000", 2).toByte,
    // 7
    Integer.parseInt("00000000", 2).toByte,
    Integer.parseInt("01111111", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("00000110", 2).toByte,
    Integer.parseInt("00001100", 2).toByte,
    Integer.parseInt("00011000", 2).toByte,
    Integer.parseInt("00110000", 2).toByte,
    Integer.parseInt("01100000", 2).toByte,
    Integer.parseInt("11000000", 2).toByte,
    Integer.parseInt("00000000", 2).toByte,
    // 8
    Integer.parseInt("00000000", 2).toByte,
    Integer.parseInt("01111110", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("01111110", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("01111110", 2).toByte,
    Integer.parseInt("00000000", 2).toByte,
    // 9
    Integer.parseInt("00000000", 2).toByte,
    Integer.parseInt("01111110", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("11000011", 2).toByte,
    Integer.parseInt("01111111", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("00000011", 2).toByte,
    Integer.parseInt("01111110", 2).toByte,
    Integer.parseInt("00000000", 2).toByte
  )
}
