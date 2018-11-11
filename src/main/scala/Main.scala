import java.nio.ByteBuffer
import scala.language.implicitConversions

object Implicits {
  implicit def intToShort(i: Int): Short = {
    val highByte: Byte = ((i & 0xFF00) >> 8).toByte
    val lowByte: Byte = (i & 0xFF).toByte
    ByteBuffer.wrap(Array[Byte](highByte, lowByte)).getShort
  }

  implicit def intToByte(i: Int): Byte = {
    val lowByte: Byte = (i & 0xFF).toByte
    ByteBuffer.wrap(Array[Byte](0x00, lowByte)).get(1)
  }

  implicit def shortToByte(i: Short): Byte = {
    val lowByte: Byte = (i & 0xFF).toByte
    ByteBuffer.wrap(Array[Byte](0x00, lowByte)).get(1)
  }
}

object Main extends App {
  val programName = "demo.ch8"

  println("+++ CHIP8 Emulator started +++")
  val memory = new Memory
  println("Testing memory")
  if (!MemoryTest.test(memory)) {
    println("Memory test failed")
    System.exit(1)
  }

  println("Loading interpreter")
  val interpreter = new Interpreter(memory)
  interpreter.loadSelf()

  println("Loading " + programName)
  val programSize = interpreter
    .loadProgram(programName)
    .getOrElse {
      System.exit(1)
      0
  }

  println("Loaded " + programName + " : " + programSize + " bytes" )
  val display = new Display(memory)

  val millisecondsSleep = 2
  var countUntilDecrement = 12

  val cpu = new Cpu(memory)
  while(true) {
    val instruction = cpu.fetch
    cpu.execute(instruction)
    display.update()

    Thread.sleep(millisecondsSleep)
    countUntilDecrement -= 1
    if (countUntilDecrement == 0) {
      cpu.decrementDelay
      countUntilDecrement = 12
    }
  }
}