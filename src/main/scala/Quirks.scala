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

object Quirks {
  /**
    * See: http://mir3z.github.io/chip8-emu/doc/#toc1
    *
    * Apparently some roms make wrong assumptions, for some roms to work properly
    * these two values might have to change so the CPU behaves slightly differently
    * according to what these roms assume.
    *
    * Load/store quirks - Instructions LD [I], Vx and LD Vx, [I] increments value of I
    * register but some CHIP-8 programs assumes that they don't.
    *
    * Shift quirks - Shift instructions originally shift register VY and store results
    * in register VX. Some CHIP-8 programs incorrectly assumes that the VX register is
    * shifted by this instruction, and VY remains unmodified.
    */

  // When storing and reading the v0..x registers the I register should be incremented by x
  // after reading or writing, however many roms do not like this and expect this not to happen.
  val increment_I = false

  // When SHL or SHR the vY register should be shifted and stored inside vX, however many roms
  // assume incorrectly that vX is shifted instead.
  val shift_vY = false
}
