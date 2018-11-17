import java.nio.ByteBuffer
import scala.language.implicitConversions

object Implicits {
  implicit def intToShort(i: Int): Short = {
    val highByte: Byte = (i & 0xFF00) >> 8
    val lowByte: Byte = i & 0xFF
    ByteBuffer.wrap(Array[Byte](highByte, lowByte)).getShort
  }

  implicit def intToByte(i: Int): Byte = {
    val lowByte: Byte = (i % 256).toByte
    ByteBuffer.wrap(Array[Byte](0x00, lowByte)).get(1)
  }

  implicit def shortToByte(i: Short): Byte = {
    val lowByte: Byte = i & 0xFF
    ByteBuffer.wrap(Array[Byte](0x00, lowByte)).get(1)
  }

  implicit def byteToShort(i: Byte): Short = {
    ByteBuffer.wrap(Array[Byte](0x00, i)).getShort
  }
}

object Main extends App {
  val programName = "tetris.ch8"

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
  val controller = new Controller
  val display = new Display(memory, controller)
  display.start()

  var lastDelayDecrement = System.currentTimeMillis()

  val cpu = new Cpu(memory, controller)

  val delayThread = new Thread {
    override def run(): Unit = {
      while(!isInterrupted) {
        val elapsed = System.currentTimeMillis() - lastDelayDecrement
        if (elapsed >= 16) {
          cpu.decrementDelay
          lastDelayDecrement = System.currentTimeMillis()
        }
      }
    }
  }

  delayThread.start()

  val frequency = 400

  while(display.isRunning) {
    cpu.execute(cpu.fetch)
    Thread.sleep(1000/frequency)
  }

  delayThread.interrupt()
  delayThread.join()

  println("Finished")
  System.exit(0)
}