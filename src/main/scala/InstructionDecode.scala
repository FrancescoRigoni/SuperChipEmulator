
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

trait InstructionDecode {
  def getXandKK(instruction:Short) : (Byte, Byte) = {
    val x: Byte = (instruction & 0x0F00) >> 8
    val kk: Byte = instruction & 0x00FF
    (x, kk)
  }

  def getX(instruction:Short) : Byte = {
    (instruction & 0x0F00) >> 8
  }

  def getXandY(instruction:Short) : (Byte, Byte) = {
    val x: Byte = (instruction & 0x0F00) >> 8
    val y: Byte = (instruction & 0x00F0) >> 4
    (x, y)
  }

  def getXYN(instruction:Short) : (Byte, Byte, Byte) = {
    val x: Byte = (instruction & 0x0F00) >> 8
    val y: Byte = (instruction & 0x00F0) >> 4
    val n: Byte = instruction & 0x000F
    (x, y, n)
  }

  def getNNN(instruction:Short) : Short = {
    instruction & 0x0FFF
  }

  def getN(instruction:Short) : Byte = {
    instruction & 0xF
  }

  def decode(instr : Short) : Unit = {
    instr match {
      case v if v == 0x00EE => ret()
      case v if v == 0x00E0 => cls()
      case v if v == 0x00FB => scr()
      case v if v == 0x00FD => exit()
      case v if v == 0x00FD => high_res_on(false)
      case v if v == 0x00FF => high_res_on(true)
      case v if v == 0x00FC => scl()
      case v if (v & 0xFFF0) == 0x00B0 => scu_n(byteToInt(getN(v)))
      case v if (v & 0xFFF0) == 0x00C0 => scd_n(byteToInt(getN(v)))
      case v if (v & 0xF0FF) == 0xE09E => skp_Vx(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xE0A1 => sknp_Vx(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xF007 => ld_Vx_DT(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xF00A => ld_Vx_K(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xF015 => ld_DT_Vx(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xF018 => ld_ST_Vx(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xF01E => add_I_Vx(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xF029 => ld_F_Vx_8_by_5(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xF030 => ld_F_Vx_8_by_5(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xF033 => ld_B_Vx(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xF055 => ld_I_Vx(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xF065 => ld_Vx_I(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xF075 => ld_r_Vx(byteToInt(getX(v)))
      case v if (v & 0xF0FF) == 0xF085 => ld_Vx_r(byteToInt(getX(v)))
      case v if (v & 0xF00F) == 0x8000 => ld_Vx_Vy(bytesToInts(getXandY(v)))
      case v if (v & 0xF00F) == 0x8001 => or_Vx_Vy(bytesToInts(getXandY(v)))
      case v if (v & 0xF00F) == 0x8002 => and_Vx_Vy(bytesToInts(getXandY(v)))
      case v if (v & 0xF00F) == 0x8003 => xor_Vx_Vy(bytesToInts(getXandY(v)))
      case v if (v & 0xF00F) == 0x8004 => add_Vx_Vy(bytesToInts(getXandY(v)))
      case v if (v & 0xF00F) == 0x8005 => sub_Vx_Vy(bytesToInts(getXandY(v)))
      case v if (v & 0xF00F) == 0x8006 => shr_Vx(bytesToInts(getXandY(v)))
      case v if (v & 0xF00F) == 0x8007 => sub_Vy_Vx(bytesToInts(getXandY(v)))
      case v if (v & 0xF00F) == 0x800E => shl_Vx(bytesToInts(getXandY(v)))
      case v if (v & 0xF00F) == 0x9000 => sne_Vx_Vy(bytesToInts(getXandY(v)))
      case v if (v & 0xF000) == 0x1000 => jp_nnn(getNNN(v))
      case v if (v & 0xF000) == 0x2000 => call_nnn(getNNN(v))
      case v if (v & 0xF000) == 0x3000 => se_Vx_kk(bytesToInts(getXandKK(v)))
      case v if (v & 0xF000) == 0x4000 => sne_Vx_kk(bytesToInts(getXandKK(v)))
      case v if (v & 0xF000) == 0x5000 => se_Vx_Vy(bytesToInts(getXandY(v)))
      case v if (v & 0xF000) == 0x6000 => ld_Vx_kk(getXandKK(v))
      case v if (v & 0xF000) == 0x7000 => add_Vx_kk(getXandKK(v))
      case v if (v & 0xF000) == 0xA000 => ld_I_nnn(getNNN(v))
      case v if (v & 0xF000) == 0xB000 => jp_V0_nnn(getNNN(v))
      case v if (v & 0xF000) == 0xC000 => rnd_Vx_kk(bytesToInts(getXandKK(v)))
      case v if (v & 0xF000) == 0xD000 => drw_Vx_Vy_n(bytesToInts(getXYN(v)))
      case _ => unimplemented
    }
  }

  def ret()
  def cls()
  def scr()
  def scl()
  def exit()
  def high_res_on(enabled: Boolean)
  def scu_n(n: Int)
  def scd_n(n: Int)
  def skp_Vx(x: Int)
  def sknp_Vx(x: Int)
  def ld_Vx_DT(x: Int)
  def ld_DT_Vx(x: Int)
  def ld_Vx_K(x: Int)
  def ld_ST_Vx(x: Int)
  def add_I_Vx(x: Int)
  def ld_F_Vx_8_by_5(x: Int)
  def ld_F_Vx_8_by_10(x: Int)
  def ld_B_Vx(x: Int)
  def ld_I_Vx(x: Int)
  def ld_Vx_I(x: Int)
  def ld_r_Vx(x: Int)
  def ld_Vx_r(x: Int)
  def ld_Vx_Vy(xy: (Int, Int))
  def or_Vx_Vy(xy: (Int, Int))
  def and_Vx_Vy(xy: (Int, Int))
  def xor_Vx_Vy(xy: (Int, Int))
  def add_Vx_Vy(xy: (Int, Int))
  def sub_Vx_Vy(xy: (Int, Int))
  def sub_Vy_Vx(xy: (Int, Int))
  def shr_Vx(xy: (Int, Int))
  def shl_Vx(xy: (Int, Int))
  def sne_Vx_Vy(xy: (Int, Int))
  def se_Vx_Vy(xy: (Int, Int))
  def jp_nnn(nnn: Short)
  def call_nnn(nnn: Short)
  def sne_Vx_kk(xkk: (Int, Int))
  def se_Vx_kk(xkk: (Int, Int))
  def ld_Vx_kk(xkk: (Int, Int))
  def add_Vx_kk(xkk: (Int, Int))
  def ld_I_nnn(nnn: Short)
  def jp_V0_nnn(nnn: Short)
  def rnd_Vx_kk(xkk: (Int, Int))
  def drw_Vx_Vy_n(xyn: (Int, Int, Int))
  def unimplemented
}