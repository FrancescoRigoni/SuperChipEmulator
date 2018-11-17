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

import java.util.NoSuchElementException

class Controller() {
  val keys = Array.ofDim[Boolean](16)
  val keyMap = Map(
    '0' -> 0,
    '1' -> 1,
    '2' -> 2,
    '3' -> 3,
    '4' -> 4,
    '5' -> 5,
    '6' -> 6,
    '7' -> 7,
    '8' -> 8,
    '9' -> 9,
    'a' -> 10,
    'b' -> 11,
    'c' -> 12,
    'd' -> 13,
    'e' -> 14,
    'f' -> 15,
  )

  def isAnyKeyPressed() : Boolean = synchronized {
    keys.fold(false)(_ | _)
  }

  def setKeyPressed(char: Char, pressed: Boolean): Unit = synchronized {
    var index = 0
    try {
      index = keyMap(char)
      keys(index) = pressed
    } catch {
      case _: NoSuchElementException => {}
    }
  }

  def isKeyPressed(key: Int): Boolean = {
    keys(key)
  }

  def getKeyPressed() : Int = {
    keys.indexWhere(_ == true)
  }
}

