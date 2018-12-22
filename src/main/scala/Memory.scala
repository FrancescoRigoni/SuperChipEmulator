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

import Implicits._

object Memory {
  val RAM_SIZE = 4096
  val PROGRAM_START = 0x200.toShort
  val VIDEO_WIDTH = 64
  val VIDEO_HEIGHT = 32
  val SPRITE_WIDTH = 8
}

class Memory {
  private val ram = Array.ofDim[Byte](Memory.RAM_SIZE)
  private var storingPointer:Short = Memory.RAM_SIZE
  var video = Array.ofDim[Boolean](Memory.VIDEO_HEIGHT, Memory.VIDEO_WIDTH)

  def ramStartStoring(from:Short) : Unit = {
    storingPointer = from
  }

  def ramFinishStoring() : Unit = {
    storingPointer = Memory.RAM_SIZE
  }

  def ramStoreByte(byte:Byte) : Unit = {
    ram(storingPointer) = byte
    storingPointer += 1
  }

  def ramReadByte(address:Short) : Byte = {
    ram(address)
  }

  def ramReadTwoBytes(address:Short) : Short = {
    val highByte = ram(address)
    val lowByte = ram(address + 1)
    (((highByte << 8) & 0xFF00) | (lowByte & 0xFF)).toShort
  }

  def clearVideo() : Unit = synchronized {
    for(x <- 0 until Memory.VIDEO_WIDTH) {
      for(y <- 0 until Memory.VIDEO_HEIGHT) {
        video(y)(x) = false
      }
    }
  }

  def scrollUp(lines: Int) : Unit = {
    val newVideo = Array.fill[Boolean](Memory.VIDEO_HEIGHT, Memory.VIDEO_WIDTH)(false)
    for (line <- lines until Memory.VIDEO_HEIGHT) {
      newVideo(line - lines) = video(line)
    }
    video = newVideo
  }

  def scrollDown(lines: Int) : Unit = {
    val newVideo = Array.fill[Boolean](Memory.VIDEO_HEIGHT, Memory.VIDEO_WIDTH)(false)
    for (line <- 0 until Memory.VIDEO_HEIGHT - lines) {
      newVideo(line) = video(line + lines)
    }
    video = newVideo
  }

  def scrollLeft(columns: Int) : Unit = {
    val newVideo = Array.fill[Boolean](Memory.VIDEO_HEIGHT, Memory.VIDEO_WIDTH)(false)
    for (y <- 0 until Memory.VIDEO_HEIGHT) {
      for (x <- 0 until Memory.VIDEO_WIDTH - columns) {
        newVideo(y)(x + columns) = video(y)(x)
      }
    }
    video = newVideo
  }

  def scrollRight(columns: Int) : Unit = {
    val newVideo = Array.fill[Boolean](Memory.VIDEO_HEIGHT, Memory.VIDEO_WIDTH)(false)
    for (y <- 0 until Memory.VIDEO_HEIGHT) {
      for (x <- columns until Memory.VIDEO_WIDTH) {
        newVideo(y)(x - columns) = video(y)(x)
      }
    }
    video = newVideo
  }

  def showSprite(height:Int, fromAddress: Int, xCoord: Byte, yCoord: Byte) : Boolean = synchronized {
    // println("Draw sprite at " + xCoord + ":" + yCoord + " fromMem: " + fromAddress + " size: " + height)
    var collisions = 0
    var sourceAddress = fromAddress

    for (y <- yCoord until yCoord + height) {
      val positiveY:Byte = y % Memory.VIDEO_HEIGHT

      for (x <- xCoord until xCoord + 8) {
        val positiveX:Byte = x % Memory.VIDEO_WIDTH

        //val locationInVideoMemory:Short = (positiveY * Memory.VIDEO_WIDTH) + positiveX
        val currentRow = ramReadByte(sourceAddress)
        val currentPx = x - xCoord
        val mask:Byte = 0x80 >> currentPx
        val pixelShouldBeOn = (currentRow & mask) != 0

        val current = video(positiveY)(positiveX)
        if (current && pixelShouldBeOn) collisions += 1
        video(positiveY)(positiveX) ^= pixelShouldBeOn
      }

      sourceAddress += 1
    }

    collisions > 0
  }
}
