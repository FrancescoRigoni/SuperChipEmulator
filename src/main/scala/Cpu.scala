
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

import java.io.FileWriter
import java.util.Random
import java.util.concurrent.atomic.AtomicInteger

import Implicits._

class Cpu(private val memory: Memory, private val controller: Controller) extends InstructionDecode {
  val regHPRPL = Array.ofDim[Byte](8)
  val regVX = Array.ofDim[Byte](16)
  var regI:Short = _
  var regSound:Byte = _
  val regDelay:AtomicInteger = new AtomicInteger()
  var regPC = Memory.PROGRAM_START
  var stack = List[Short]()

  def decrementDelay : Unit = {
    if (regDelay.get() > 0) regDelay.set(regDelay.get()-1)
  }

  private def pop : Short = {
    val value = stack.head
    stack = stack.tail
    value
  }

  private def push(value:Short) = {
    stack = value :: stack
  }

  def fetch : Short = {
    val value = memory.ramReadTwoBytes(regPC)
    regPC = regPC + 2
    value
  }

  private def generateRandom() : Byte = {
    new Random().nextInt()
  }

  def execute(instr : Short) : Unit = {
    decode(instr)

    // DEBUG LOGS
    if (EmulatorParameters.DEBUG_CPU) {
      print("Executing " + f"$instr%X" + " at " + (regPC-2) + " ")
      print("I: " + regI + " R: [")
      for (reg <- regVX) {
        print(" " + reg)
      }
      println("]")
    }
  }

  override def ret(): Unit = {
    regPC = pop
  }

  override def cls(): Unit = {
    memory.clearVideo()
  }

  override def scr(): Unit = {
    // Scroll right 4 pixels (2 pixels in low res mode)
    memory.scrollRight(if (memory.isHighRes()) 4 else 2)
  }

  override def scl(): Unit = {
    // Scroll left 4 pixels (2 pixels in low res mode)
    memory.scrollLeft(if (memory.isHighRes()) 4 else 2)
  }

  override def exit(): Unit = {
    // Exit, infinite loop
    regPC -= 2
  }

  override def high_res_on(enabled: Boolean): Unit = {
    // Enter low/high resolution (64x32)/(128x64) mode
    memory.setHighResolution(enabled)
  }

  override def scu_n(n: Int): Unit = {
    // Scroll up N pixels (N/2 pixels in low res mode)
    memory.scrollUp(if (memory.isHighRes()) n else n / 2)
  }

  override def scd_n(n: Int): Unit = {
    // Scroll down N pixels (N/2 pixels in low res mode)
    memory.scrollDown(if (memory.isHighRes()) n else n / 2)
  }

  override def skp_Vx(x: Int): Unit = {
    // Skip next instruction if key with the value of Vx is pressed
    if (controller.isKeyPressed(regVX(x))) regPC = regPC + 2
  }

  override def sknp_Vx(x: Int): Unit = {
    // Skip next instruction if key with the value of Vx is not pressed
    if (!controller.isKeyPressed(regVX(x))) regPC = regPC + 2
  }

  override def ld_Vx_DT(x: Int): Unit = {
    // Set Vx = delay timer value
    regVX(x) = intToByte(regDelay.get())
  }

  override def ld_DT_Vx(x: Int): Unit = {
    // Set delay timer = Vx
    regDelay.set(byteToInt(regVX(x)))
  }

  override def ld_Vx_K(x: Int): Unit = {
    // Wait for a key press, store the value of the key in Vx
    if (!controller.isAnyKeyPressed()) regPC = regPC - 2
    else regVX(x) = controller.getKeyPressed()
  }

  override def ld_ST_Vx(x: Int): Unit = {
    // Set sound timer = Vx
    regSound = regVX(x)
  }

  override def add_I_Vx(x: Int): Unit = {
    // Set I = I + Vx
    regI = regI + byteToShort(regVX(x))
  }

  override def ld_F_Vx_8_by_5(x: Int): Unit = {
    // Set I = location of sprite for digit Vx
    regI = byteToShort(regVX(x)) * 5
  }

  override def ld_F_Vx_8_by_10(x: Int): Unit = {
    // Set I = location of sprite for digit Vx
    regI = (byteToShort(regVX(x)) * 10) + 0x100
  }

  override def ld_B_Vx(x: Int): Unit = {
    // Store BCD representation of Vx in memory locations I, I+1, and I+2
    val hundreds = byteToInt(regVX(x)) / 100
    val tenths = (byteToInt(regVX(x)) % 100) / 10
    val units = (byteToInt(regVX(x)) % 100) % 10
    memory.ramStartStoring(shortToInt(regI))
    memory.ramStoreByte(hundreds)
    memory.ramStoreByte(tenths)
    memory.ramStoreByte(units)
    memory.ramFinishStoring()
  }

  override def ld_I_Vx(x: Int): Unit = {
    // Store registers V0 through Vx in memory starting at location I
    memory.ramStartStoring(shortToInt(regI))
    (0 to x).foreach { p => memory.ramStoreByte(regVX(p)) }
    memory.ramFinishStoring()
    if (Quirks.increment_I) {
      regI += intToShort(x)
    }
  }

  override def ld_Vx_I(x: Int): Unit = {
    // Read registers V0 through Vx from memory starting at location I
    (0 to x).foreach { p => regVX(p) = memory.ramReadByte(regI + p) }
    if (Quirks.increment_I) {
      regI += intToShort(x)
    }
  }

  override def ld_r_Vx(x: Int): Unit = {
    // Store V0..VX (inclusive) into HP-RPL user flags R0..RX (X < 8)
    for (n <- 0 until x) {
      regHPRPL(n) = regVX(n)
    }
  }

  override def ld_Vx_r(x: Int): Unit = {
    // Load V0..VX (inclusive) from HP-RPL user flags R0..RX (X < 8)
    for (n <- 0 until x) {
      regVX(n) = regHPRPL(n)
    }
  }

  override def ld_Vx_Vy(xy: (Int, Int)): Unit = {
    // Set Vx = Vy
    regVX(xy._1) = regVX(xy._2)
  }

  override def or_Vx_Vy(xy: (Int, Int)): Unit = {
    // Set Vx = Vx OR Vy
    regVX(xy._1) = regVX(xy._1) | regVX(xy._2)
  }

  override def and_Vx_Vy(xy: (Int, Int)): Unit = {
    // Set Vx = Vx AND Vy
    regVX(xy._1) = regVX(xy._1) & regVX(xy._2)
  }

  override def xor_Vx_Vy(xy: (Int, Int)): Unit = {
    // Set Vx = Vx XOR Vy
    regVX(xy._1) = regVX(xy._1) ^ regVX(xy._2)
  }

  override def add_Vx_Vy(xy: (Int, Int)): Unit = {
    // Vx = Vx + Vy, set VF = carry
    val regX = byteToInt(regVX(xy._1))
    val regY = byteToInt(regVX(xy._2))
    val addition = regX + regY
    regVX(xy._1) = intToByte(addition)
    regVX(0xF) = if (addition > 255) 1 else 0
  }

  override def sub_Vx_Vy(xy: (Int, Int)): Unit = {
    // Set Vx = Vx - Vy, set VF = NOT borrow
    regVX(0xF) = if (regVX(xy._1) > regVX(xy._2)) 1 else 0
    val regX = byteToInt(regVX(xy._1))
    val regY = byteToInt(regVX(xy._2))
    val subtraction = regX - regY
    regVX(xy._1) = subtraction
  }

  override def sub_Vy_Vx(xy: (Int, Int)): Unit = {
    // Set Vx = Vy - Vx, set VF = NOT borrow
    regVX(0xF) = if (regVX(xy._2) > regVX(xy._1)) 1 else 0
    regVX(xy._1) = regVX(xy._2) - regVX(xy._1)
  }

  override def shr_Vx(xy: (Int, Int)): Unit = {
    // Set Vx = Vx SHR 1
    if (Quirks.shift_vY) {
      regVX(0xF) = if ((regVX(xy._2) & 0x1) == 1) 1 else 0
      regVX(xy._1) = intToByte(regVX(xy._2) >>> 1)
    } else {
      regVX(0xF) = if ((regVX(xy._1) & 0x1) == 1) 1 else 0
      regVX(xy._1) = intToByte(regVX(xy._1) >>> 1)
    }
  }

  override def shl_Vx(xy: (Int, Int)): Unit = {
    // Set Vx = Vx SHL 1
    if (Quirks.shift_vY) {
      regVX(0xF) = if ((regVX(xy._2) & 0x80) == 0x80) 1 else 0
      regVX(xy._1) = intToByte(regVX(xy._2) << 1)
    } else {
      regVX(0xF) = if ((regVX(xy._1) & 0x80) == 0x80) 1 else 0
      regVX(xy._1) = intToByte(regVX(xy._1) << 1)
    }
  }

  override def sne_Vx_Vy(xy: (Int, Int)): Unit = {
    // Skip next instruction if Vx != Vy
    if (regVX(xy._1) != regVX(xy._2)) regPC += 2
  }

  override def se_Vx_Vy(xy: (Int, Int)): Unit = {
    // Skip next instruction if Vx = Vy.
    if (regVX(xy._1) == regVX(xy._2)) regPC += 2
  }

  override def jp_nnn(nnn: Short): Unit = {
    // Jump to location nnn
    regPC = nnn
  }

  override def call_nnn(nnn: Short): Unit = {
    // Call subroutine at nnn.
    push(regPC)
    regPC = nnn
  }

  override def sne_Vx_kk(xkk: (Int, Int)): Unit = {
    // Skip next instruction if Vx != kk.
    if (byteToInt(regVX(xkk._1)) != xkk._2) regPC = regPC + 2
  }

  override def se_Vx_kk(xkk: (Int, Int)): Unit = {
    // Skip next instruction if Vx = kk
    if (byteToInt(regVX(xkk._1)) == xkk._2) regPC = regPC + 2
  }

  override def ld_Vx_kk(xkk: (Int, Int)): Unit = {
    // Set Vx = kk.
    regVX(xkk._1) = intToByte(xkk._2)
  }

  override def add_Vx_kk(xkk: (Int, Int)): Unit = {
    // Set Vx = Vx + kk
    regVX(xkk._1) = intToByte(byteToInt(regVX(xkk._1)) + xkk._2)
  }

  override def ld_I_nnn(nnn: Short): Unit = {
    // I = nnn
    regI = nnn & 0xFFF
  }

  override def jp_V0_nnn(nnn: Short): Unit = {
    // Jump to location nnn + V0
    regPC = nnn + regVX(0)
  }

  override def rnd_Vx_kk(xkk: (Int, Int)): Unit = {
    // Set Vx = random byte AND kk
    regVX(byteToInt(xkk._1)) = generateRandom() & xkk._2
  }

  override def drw_Vx_Vy_n(xyn: (Int, Int, Int)): Unit = {
    // Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.
    val xCoord:Int = byteToInt(regVX(xyn._1))
    val yCoord:Int = byteToInt(regVX(xyn._2))

    if (xyn._3 == 0 && memory.isHighRes()) {
      regVX(0xF) = if(memory.showSprite16(shortToInt(regI), xCoord, yCoord)) 1 else 0
    } else {
      regVX(0xF) = if(memory.showSprite(xyn._3, shortToInt(regI), xCoord, yCoord)) 1 else 0
    }
  }

  override def unimplemented: Unit = {
    throw new RuntimeException("Unimplemented")
  }
}