import java.nio.ByteBuffer

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
  private val video = Array.ofDim[Boolean](Memory.VIDEO_WIDTH * Memory.VIDEO_HEIGHT)
  private var storingPointer:Int = POINTER_INVALID

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

  def showSprite(size:Int, fromAddress: Int, xCoord: Int, yCoord: Int) : Boolean = {
    println("ShowSprite at " + xCoord + ":" + yCoord + " height: " + size)
    var collisions = 0
    var sourceAddress = fromAddress;

    for (y <- yCoord until yCoord + size) {
      var destinationAddress = y * Memory.VIDEO_WIDTH + xCoord + 7
      var pixelRow = ramReadByte(sourceAddress.toShort)
      for (i <- 0 until 8) {
        val isOn = (pixelRow & 1) == 1
        if (isOn && video(destinationAddress)) collisions += 1
        video(destinationAddress) ^= isOn

        pixelRow = (pixelRow >>> 1).toByte
        destinationAddress -= 1
      }
      sourceAddress += 1
    }
    dumpVideo()
    collisions > 0
  }

  def dumpVideo() : Unit = {
    for (y <- 0 until Memory.VIDEO_HEIGHT) {
      for (x <- 0 until Memory.VIDEO_WIDTH) {
        var absolutePosInVideoMemory = y * Memory.VIDEO_WIDTH + x
          if (video(absolutePosInVideoMemory)) print("X")
          else print(" ")
      }
      println("")
    }
    for (x <- 0 until Memory.VIDEO_WIDTH) print("-")
    println("")
  }
}
