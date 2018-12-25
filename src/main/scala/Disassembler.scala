import java.io.FileWriter

import scala.collection.mutable

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

import Implicits._

class Disassembler(private val memory: Memory, private val outputFileName:String) extends InstructionDecode {
  // Set of addresses to visit, initially only contains the program start address
  private var addressesToVisit = mutable.HashSet[Short](Memory.PROGRAM_START)
  // Set of already visited addresses
  private val visitedAddresses = mutable.HashSet[Short]()

  private val disassembledOutput = new StringBuilder()

  // An unknown opcode found ends the current visit of disassembly
  private var unimplementedOpCodeFound = false

  def disassemble(): Unit = {
    val outputFileWriter = new FileWriter(outputFileName)
    try {
      while (addressesToVisit.nonEmpty) {
        // Get next address to be visited
        val nextAddressToVisit = addressesToVisit.head
        // Remove it from addressesToVisit
        addressesToVisit = addressesToVisit.tail
        // Add it to already visited addresses
        visitedAddresses.add(nextAddressToVisit)
        // Go for it
        visit(nextAddressToVisit)
        // Finally dump the disassembled code to file for this round
        disassembledOutput.append("\n\n")
        outputFileWriter.write(disassembledOutput.toString())
        // Clear the buffer for the next round
        disassembledOutput.clear()
      }
    }
    finally outputFileWriter.close()
  }

  private def visit(address: Short) = {
    var currentOpCodeAddress = address
    disassembledOutput
      .append("Program segment at address: ")
      .append(asHexString(currentOpCodeAddress))
      .append("\n")
    unimplementedOpCodeFound = false
    while (!unimplementedOpCodeFound &&
           currentOpCodeAddress < Memory.RAM_SIZE) {

      val instruction = memory.ramReadTwoBytes(currentOpCodeAddress)
      disassembledOutput.append(asHexString(currentOpCodeAddress)).append("    ")
      decode(instruction)
      disassembledOutput.append("\n")
      currentOpCodeAddress += 2
    }
  }

  private def addIfNotVisitedAndNotToVisitYet(nnn: Short): Unit =
    if (!visitedAddresses.contains(nnn) &&
        !addressesToVisit.contains(nnn))
          addressesToVisit.add(nnn)

  private def asHexString(x: Int): String = f"$x%1X"
  private def asHexString(x: Short): String = asHexString(shortToInt(x))

  override def ret(): Unit = {
    disassembledOutput.append("RET")
  }

  override def cls(): Unit = {
    disassembledOutput.append("CLS")
  }

  override def skp_Vx(x: Int): Unit = {
    disassembledOutput.append("SKP V").append(asHexString(x))
  }

  override def sknp_Vx(x: Int): Unit = {
    disassembledOutput.append("SKNP V").append(asHexString(x))
  }

  override def ld_Vx_DT(x: Int): Unit = {
    disassembledOutput.append("LD V").append(asHexString(x)).append(", DT")
  }

  override def ld_DT_Vx(x: Int): Unit = {
    disassembledOutput.append("LD DT, V").append(asHexString(x))
  }

  override def ld_Vx_K(x: Int): Unit = {
    disassembledOutput.append("LD V").append(asHexString(x)).append(", K")
  }

  override def ld_ST_Vx(x: Int): Unit = {
    disassembledOutput.append("LD ST, V").append(asHexString(x))
  }

  override def add_I_Vx(x: Int): Unit = {
    disassembledOutput.append("ADD I, V").append(asHexString(x))
  }

  override def ld_F_Vx_8_by_5(x: Int): Unit = {
    disassembledOutput.append("LD F, V").append(asHexString(x))
  }

  override def ld_B_Vx(x: Int): Unit = {
    disassembledOutput.append("LD B, V").append(asHexString(x))
  }

  override def ld_I_Vx(x: Int): Unit = {
    disassembledOutput.append("LD [I], V").append(asHexString(x))
  }

  override def ld_Vx_I(x: Int): Unit = {
    disassembledOutput.append("LD V").append(asHexString(x)).append(", [I]")
  }

  override def ld_Vx_Vy(xy: (Int, Int)): Unit = {
    disassembledOutput.append("LD V").append(asHexString(xy._1)).append(", V").append(asHexString(xy._2))
  }

  override def or_Vx_Vy(xy: (Int, Int)): Unit = {
    disassembledOutput.append("OR V").append(asHexString(xy._1)).append(", V").append(asHexString(xy._2))
  }

  override def and_Vx_Vy(xy: (Int, Int)): Unit = {
    disassembledOutput.append("OR V").append(asHexString(xy._1)).append(", V").append(asHexString(xy._2))
  }

  override def xor_Vx_Vy(xy: (Int, Int)): Unit = {
    disassembledOutput.append("XOR V").append(asHexString(xy._1)).append(", V").append(asHexString(xy._2))
  }

  override def add_Vx_Vy(xy: (Int, Int)): Unit = {
    disassembledOutput.append("ADD V").append(asHexString(xy._1)).append(", V").append(asHexString(xy._2))
  }

  override def sub_Vx_Vy(xy: (Int, Int)): Unit = {
    disassembledOutput.append("SUB V").append(asHexString(xy._1)).append(", V").append(asHexString(xy._2))
  }

  override def sub_Vy_Vx(xy: (Int, Int)): Unit = {
    disassembledOutput.append("SUBN V").append(asHexString(xy._1)).append(", V").append(asHexString(xy._2))
  }

  override def shr_Vx(xy: (Int, Int)): Unit = {
    disassembledOutput.append("SHR V").append(asHexString(xy._1)).append("{, V").append(asHexString(xy._2)).append("}")
  }

  override def shl_Vx(xy: (Int, Int)): Unit = {
    disassembledOutput.append("SHL V").append(asHexString(xy._1)).append("{, V").append(asHexString(xy._2)).append("}")
  }

  override def sne_Vx_Vy(xy: (Int, Int)): Unit = {
    disassembledOutput.append("SNE V").append(asHexString(xy._1)).append(", V").append(asHexString(xy._2))
  }

  override def se_Vx_Vy(xy: (Int, Int)): Unit = {
    disassembledOutput.append("SE V").append(asHexString(xy._1)).append(", V").append(asHexString(xy._2))
  }

  override def jp_nnn(nnn: Short): Unit = {
    disassembledOutput.append("JP ").append(asHexString(nnn))
    addIfNotVisitedAndNotToVisitYet(nnn)
  }

  override def call_nnn(nnn: Short): Unit = {
    disassembledOutput.append("CALL ").append(asHexString(nnn))
    addIfNotVisitedAndNotToVisitYet(nnn)
  }

  override def sne_Vx_kk(xkk: (Int, Int)): Unit = {
    disassembledOutput.append("SNE V").append(asHexString(xkk._1)).append(", ").append(xkk._2)
  }

  override def se_Vx_kk(xkk: (Int, Int)): Unit = {
    disassembledOutput.append("SE V").append(asHexString(xkk._1)).append(", ").append(xkk._2)
  }

  override def ld_Vx_kk(xkk: (Int, Int)): Unit = {
    disassembledOutput.append("LD V").append(asHexString(xkk._1)).append(", ").append(xkk._2)
  }

  override def add_Vx_kk(xkk: (Int, Int)): Unit = {
    disassembledOutput.append("ADD V").append(asHexString(xkk._1)).append(", ").append(xkk._2)
  }

  override def ld_I_nnn(nnn: Short): Unit = {
    disassembledOutput.append("LD I, ").append(asHexString(nnn))
  }

  override def jp_V0_nnn(nnn: Short): Unit = {
    disassembledOutput.append("JP V0, ").append(asHexString(nnn))
    addIfNotVisitedAndNotToVisitYet(nnn)
  }

  override def rnd_Vx_kk(xkk: (Int, Int)): Unit = {
    disassembledOutput.append("RND V").append(asHexString(xkk._1)).append(", ").append(asHexString(xkk._2))
  }

  override def drw_Vx_Vy_n(xyn: (Int, Int, Int)): Unit = {
    disassembledOutput.append("DRW V").append(asHexString(xyn._1)).append(", V")
      .append(asHexString(xyn._2)).append(", ").append(asHexString(xyn._3))
  }

  override def unimplemented: Unit = {
    disassembledOutput.append("End of segment")
    unimplementedOpCodeFound = true
  }

  override def ld_r_Vx(x: Int): Unit = {
    disassembledOutput.append("LD R, V").append(asHexString(x))
  }

  override def ld_Vx_r(x: Int): Unit = {
    disassembledOutput.append("LD V").append(asHexString(x)).append(", R")
  }

  override def ld_F_Vx_8_by_10(x: Int): Unit = {
    disassembledOutput.append("LD HF, V").append(asHexString(x))
  }

  override def scr(): Unit = {
    disassembledOutput.append("SCR")
  }

  override def scl(): Unit = {
    disassembledOutput.append("SCL")
  }

  override def exit(): Unit = {
    disassembledOutput.append("EXIT")
  }

  override def high_res_on(enabled: Boolean): Unit = {
    disassembledOutput.append(if (enabled) "HIGH" else "LOW")
  }

  override def scu_n(n: Int): Unit = {
    disassembledOutput.append("SCU ").append(n)
  }

  override def scd_n(n: Int): Unit = {
    disassembledOutput.append("SCD ").append(n)
  }
}