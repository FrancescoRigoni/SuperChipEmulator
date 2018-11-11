import java.io.{BufferedInputStream, FileInputStream, IOException}

class Interpreter(val memory: Memory) {

  def loadSelf() : Unit = {
    memory.ramStartStoring(0)
    for (v <- fonts) memory.ramStoreByte(v)
    memory.ramFinishStoring()
  }

  def loadProgram(filePath:String) : Option[Int] = {
    var in = None: Option[FileInputStream]
    var programSize: Option[Int] = None

    try {
      in = Some(new FileInputStream(filePath))
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
    } finally {
      if (in.isDefined) in.get.close()
    }

    programSize
  }

  private val fonts = Array[Byte](
    // 0
      Integer.parseInt("11110000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("10010000", 2).toByte,
      Integer.parseInt("11110000", 2).toByte,
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
}
