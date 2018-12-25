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

object Memory {
  val RAM_SIZE = 4096
  val PROGRAM_START:Short = 0x200.toShort
  private val VIDEO_WIDTH_LRES = 64
  private val VIDEO_HEIGHT_LRES = 32
  private val VIDEO_WIDTH_HRES = 128
  private val VIDEO_HEIGHT_HRES = 64
  private val SPRITE_WIDTH = 8
}

trait VideoMemoryObserver {
  def onVideoMemorySizeChanged(highRes:Boolean)
}

class Memory {
  private val ram = Array.ofDim[Byte](Memory.RAM_SIZE)
  private var storingPointer:Int = Memory.RAM_SIZE
  private var videoHiRes = false

  var videoMemoryObserver:VideoMemoryObserver = _

  var video = Array.ofDim[Boolean](videoHeight, videoWidth)

  def videoWidth : Int = {
    if (videoHiRes) Memory.VIDEO_WIDTH_HRES else Memory.VIDEO_WIDTH_LRES
  }

  def videoHeight : Int = {
    if (videoHiRes) Memory.VIDEO_HEIGHT_HRES else Memory.VIDEO_HEIGHT_LRES
  }

  def setHighResolution(enabled: Boolean): Unit = {
    videoHiRes = enabled
    video = Array.ofDim[Boolean](videoHeight, videoWidth)
    if (videoMemoryObserver != null) {
      videoMemoryObserver.onVideoMemorySizeChanged(videoHiRes)
    }
  }

  def isHighRes() : Boolean = synchronized {
    return videoHiRes
  }

  def ramStartStoring(from:Int) : Unit = {
    storingPointer = from
  }

  def ramFinishStoring() : Unit = {
    storingPointer = Memory.RAM_SIZE
  }

  def ramStoreByte(byte:Byte) : Unit = {
    ram(storingPointer) = byte
    storingPointer += 1
  }

  def ramReadByte(address:Int) : Byte = {
    ram(address)
  }

  def ramReadTwoBytes(address:Short) : Short = {
    val highByte = ram(address.toInt)
    val lowByte = ram(address + 1)
    ((highByte << 8) & 0xFF00) | (lowByte & 0xFF)
  }

  def clearVideo() : Unit = synchronized {
    for(x <- 0 until videoWidth) {
      for(y <- 0 until videoHeight) {
        video(y)(x) = false
      }
    }
  }

  def scrollUp(lines: Int) : Unit = {
    val newVideo = Array.fill[Boolean](videoHeight, videoWidth)(false)
    for (line <- lines until videoHeight) {
      newVideo(line - lines) = video(line)
    }
    video = newVideo
  }

  def scrollDown(lines: Int) : Unit = {
    val newVideo = Array.fill[Boolean](videoHeight, videoWidth)(false)
    for (line <- 0 until videoHeight - lines) {
      newVideo(line) = video(line + lines)
    }
    video = newVideo
  }

  def scrollLeft(columns: Int) : Unit = {
    val newVideo = Array.fill[Boolean](videoHeight, videoWidth)(false)
    for (y <- 0 until videoHeight) {
      for (x <- 0 until videoWidth - columns) {
        newVideo(y)(x + columns) = video(y)(x)
      }
    }
    video = newVideo
  }

  def scrollRight(columns: Int) : Unit = {
    val newVideo = Array.fill[Boolean](videoHeight, videoWidth)(false)
    for (y <- 0 until videoHeight) {
      for (x <- columns until videoWidth) {
        newVideo(y)(x - columns) = video(y)(x)
      }
    }
    video = newVideo
  }

  def showSprite(height:Int, fromAddress: Int, xCoord: Int, yCoord: Int, xInterleave: Int = 1) : Boolean = synchronized {
    //println("Draw sprite at " + xCoord + ":" + yCoord + " fromMem: " + fromAddress + " size: " + height)
    var collisions = 0
    var sourceAddress = fromAddress

    for (y <- yCoord until yCoord + height) {
      val positiveY = (y & 0xFF) % videoHeight

      for (x <- xCoord until xCoord + 8) {
        val positiveX = (x & 0xFF) % videoWidth
        val currentRow = ramReadByte(sourceAddress)
        val currentPx = x - xCoord
        val mask:Byte = 0x80 >> currentPx
        val pixelShouldBeOn = (currentRow & mask) != 0

        val current = video(positiveY)(positiveX)
        if (current && pixelShouldBeOn) collisions += 1
        video(positiveY)(positiveX) ^= pixelShouldBeOn
      }

      sourceAddress += xInterleave
    }

    collisions > 0
  }

  def showSprite16(fromAddress: Int, xCoord: Int, yCoord: Int) : Boolean = synchronized {
    showSprite(16, fromAddress, xCoord, yCoord, 2) | showSprite(16, fromAddress+1, xCoord+8, yCoord, 2)
  }
}
