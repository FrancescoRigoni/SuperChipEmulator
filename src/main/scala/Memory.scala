import java.nio.ByteBuffer
import Implicits._

object MemoryTest {
  def test(memory:Memory): Boolean = {
    memory.ramStartStoring(0)
    memory.ramStoreByte(1)
    memory.ramStoreByte(2)
    memory.ramStoreByte(3)
    memory.ramFinishStoring()

    val first = memory.ramReadByte(0)
    val second = memory.ramReadTwoBytes(1)
    first == 1 && second == 0x0203
  }
}

object Memory {
  val PROGRAM_START:Short = 0x200
  val VIDEO_WIDTH = 64
  val VIDEO_HEIGHT = 32
  val SPRITE_WIDTH = 8
}

class Memory {
  private val POINTER_INVALID:Short = 4096
  private val ram = Array.ofDim[Byte](POINTER_INVALID)
  val video = Array.ofDim[Boolean](Memory.VIDEO_WIDTH * Memory.VIDEO_HEIGHT)
  private var storingPointer:Short = POINTER_INVALID

  def ramStartStoring(from:Short) : Unit = {
    storingPointer = from
  }

  def ramFinishStoring() : Unit = {
    storingPointer = POINTER_INVALID
  }

  def ramStoreByte(byte:Byte) : Unit = {
    ram(storingPointer) = byte
    storingPointer += 1
  }

  def ramReadByte(address:Short) : Byte = {
    ram(address)
  }

  def ramReadTwoBytes(address:Short) : Short = {
    val highByte:Byte = ram(address)
    val lowByte:Byte = ram(address + 1)
    ByteBuffer.wrap(Array[Byte](highByte, lowByte)).getShort
  }

  def clearVideo() : Unit = {
    for(i <- video.indices) {
      video(i) = false
    }
  }

  def showSprite(height:Int, fromAddress: Int, xCoord: Byte, yCoord: Byte) : Boolean = {
    var collisions = 0
    var sourceAddress = fromAddress

    if (xCoord < 0 || yCoord < 0) {
      return false
    }

    if (xCoord >= Memory.VIDEO_WIDTH || yCoord >= Memory.VIDEO_HEIGHT) {
      return false
    }

    for (y <- yCoord until yCoord + height) {
      val positiveY:Byte = y % Memory.VIDEO_HEIGHT

      for (x <- xCoord until xCoord + 8) {
        val positiveX:Byte = x % Memory.VIDEO_WIDTH

        val locationInVideoMemory:Int = (positiveY * Memory.VIDEO_WIDTH) + positiveX
        val currentRow = ramReadByte(sourceAddress)
        val currentPx = x - xCoord
        val mask:Byte = 0x80 >> currentPx
        val pixelShouldBeOn = (currentRow & mask) != 0
        val current = video(locationInVideoMemory)
        if (current && pixelShouldBeOn) collisions += 1
        video(locationInVideoMemory) ^= pixelShouldBeOn
      }

      sourceAddress += 1
    }

    collisions > 0
  }
}
