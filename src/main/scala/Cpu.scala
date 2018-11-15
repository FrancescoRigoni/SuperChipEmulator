
import Implicits._

class Cpu(private val memory: Memory, private val controller: Controller) {

  private val regVX = Array.ofDim[Byte](16)
  private var regI:Short = _

  private var regSound:Byte = _
  private var regDelay:Byte = _

  private var regPC:Int = Memory.PROGRAM_START
  private var regSP:Byte = _

  private var stack = List[Short]()

  private var random = 123
  private val randomIncrement = 78

  def decrementDelay : Unit = {
    if (regDelay != 0) regDelay -= 1
  }

  private def pop : Short = {
    val value = stack.head
    stack = stack.tail
    value
  }

  private def push(value:Short) = {
    stack = value :: stack
  }

  def fetch : Int = {
    val value = memory.ramReadTwoBytes(regPC)
    regPC = regPC + 2
    value
  }

  private def getXandKK(instruction:Short) : (Byte, Byte) = {
    val x: Byte = (instruction & 0x0F00) >> 8
    val kk: Byte = instruction & 0x00FF
    (x, kk)
  }

  private def getX(instruction:Short) : Byte = {
    (instruction & 0x0F00) >> 8
  }

  private def getXandY(instruction:Short) : (Byte, Byte) = {
    val x: Byte = (instruction & 0x0F00) >> 8
    val y: Byte = (instruction & 0x00F0) >> 4
    (x, y)
  }

  private def getXYN(instruction:Short) : (Byte, Byte, Byte) = {
    val x: Byte = (instruction & 0x0F00) >> 8
    val y: Byte = (instruction & 0x00F0) >> 4
    val n: Byte = instruction & 0x000F
    (x, y, n)
  }

  private def getNNN(instruction:Short) : Short = {
    instruction & 0x0FFF
  }

  private def generateRandom() : Byte = {
    random = random + randomIncrement
    random
  }

  def execute(instruction : Int) : Unit = {
    val instr:Short = instruction
    val realPC = regPC - 0x200

    instr match {
      case v if v == 0x00EE =>
        // Return from subroutine
        regPC = pop

      case v if v == 0x00E0 =>
        // Clear display
        memory.clearVideo()

      case v if (v & 0xF0FF) == 0xE09E =>
        // Skip next instruction if key with the value of Vx is pressed
        val x = getX(v)
        if (controller.isKeyPressed(regVX(x))) regPC = regPC + 2

      case v if (v & 0xF0FF) == 0xE0A1 =>
        // Skip next instruction if key with the value of Vx is not pressed
        val x = getX(v)
        if (!controller.isKeyPressed(regVX(x))) regPC = regPC + 2

      case v if (v & 0xF0FF) == 0xF007 =>
        // Set Vx = delay timer value
        val x = getX(v)
        regVX(x) = regDelay

      case v if (v & 0xF0FF) == 0xF00A =>
        // Wait for a key press, store the value of the key in Vx
        if (!controller.isAnyKeyPressed()) regPC = regPC - 2
        else {
          val x = getX(v)
          regVX(x) = controller.getKeyPressed()
        }

      case v if (v & 0xF0FF) == 0xF015 =>
        // Set delay timer = Vx
        val x = getX(v)
        regDelay = regVX(x)

      case v if (v & 0xF0FF) == 0xF018 =>
        // Set sound timer = Vx
        val x = getX(v)
        regSound = regVX(x)

      case v if (v & 0xF0FF) == 0xF01E =>
        // Set I = I + Vx
        val x = getX(v)
        regI = regI + regVX(x)

      case v if (v & 0xF0FF) == 0xF029 =>
        // Set I = location of sprite for digit Vx
        val x = getX(v)
        regI = regVX(x) * 5

      case v if (v & 0xF0FF) == 0xF033 =>
        // Store BCD representation of Vx in memory locations I, I+1, and I+2
        val x = getX(v)
        val hundreds = regVX(x) / 100
        val tenths = (regVX(x) % 100) / 10
        val units = (regVX(x) % 100) % 10
        memory.ramStartStoring(regI)
        memory.ramStoreByte(hundreds)
        memory.ramStoreByte(tenths)
        memory.ramStoreByte(units)
        memory.ramFinishStoring()

      case v if (v & 0xF0FF) == 0xF055 =>
        // Store registers V0 through Vx in memory starting at location I
        val x = getX(v)
        memory.ramStartStoring(regI)
        (0 to x).foreach { p => memory.ramStoreByte(regVX(p)) }
        memory.ramFinishStoring()
        regI += x + 1

      case v if (v & 0xF0FF) == 0xF065 =>
        // Read registers V0 through Vx from memory starting at location I
        val x = getX(v)
        (0 to x).foreach { p => regVX(p) = memory.ramReadByte(regI + p) }
        regI += x + 1

      case v if (v & 0xF000) == 0x1000 =>
        // Jump to location nnn
        val valu = getNNN(v)
        regPC = valu

      case v if (v & 0xF000) == 0x2000 =>
        // Call subroutine at nnn.
        push(regPC)
        regPC = getNNN(v)

      case v if (v & 0xF000) == 0x3000 =>
        // Skip next instruction if Vx = kk
        val (x, kk) = getXandKK(v)
        if (regVX(x) == kk) regPC = regPC + 2

      case v if (v & 0xF000) == 0x4000 =>
        // Skip next instruction if Vx != kk.
        val (x, kk) = getXandKK(v)
        if (regVX(x) != kk) regPC = regPC + 2

      case v if (v & 0xF000) == 0x5000 =>
        // Skip next instruction if Vx = Vy.
        val (x, y) = getXandY(v)
        if (regVX(x) == regVX(y)) regPC += 2

      case v if (v & 0xF000) == 0x6000 =>
        // Set Vx = kk.
        val (x, kk) = getXandKK(v)
        regVX(x) = kk

      case v if (v & 0xF000) == 0x7000 =>
        // Set Vx = Vx + kk
        val (x, kk) = getXandKK(v)
        regVX(x) = regVX(x) + kk

      case v if (v & 0xF00F) == 0x8000 =>
        // Set Vx = Vy
        val (x, y) = getXandY(v)
        regVX(x) = regVX(y)

      case v if (v & 0xF00F) == 0x8001 =>
        // Set Vx = Vx OR Vy
        val (x, y) = getXandY(v)
        regVX(x) = regVX(x) | regVX(y)

      case v if (v & 0xF00F) == 0x8002 =>
        // Set Vx = Vx AND Vy
        val (x, y) = getXandY(v)
        regVX(x) = regVX(x) & regVX(y)

      case v if (v & 0xF00F) == 0x8003 =>
        // Set Vx = Vx XOR Vy
        val (x, y) = getXandY(v)
        regVX(x) = regVX(x) ^ regVX(y)

      case v if (v & 0xF00F) == 0x8004 =>
        // Vx = Vx + Vy, set VF = carry
        val (x, y) = getXandY(v)
        val regX:Int = regVX(x)
        val regY:Int = regVX(y)
        val addition:Int = regX + regY
        regVX(x) = addition
        regVX(0xF) = if (addition > 255) 1 else 0

      case v if (v & 0xF00F) == 0x8005 =>
        // Set Vx = Vx - Vy, set VF = NOT borrow
        val (x, y) = getXandY(v)
        regVX(0xF) = if (regVX(x) > regVX(y)) 1 else 0
        val regX:Int = regVX(x)
        val regY:Int = regVX(y)
        val subtraction:Int = regX - regY
        regVX(x) = subtraction

      case v if (v & 0xF00F) == 0x8006 =>
        // Set Vx = Vy SHR 1
        val (x, y) = getXandY(v)
        regVX(0xF) = if ((regVX(y) & 0x1) == 1) 1 else 0
        regVX(x) = regVX(y) >>> 1

      case v if (v & 0xF00F) == 0x8007 =>
        // Set Vx = Vy - Vx, set VF = NOT borrow
        val (x, y) = getXandY(v)
        regVX(0xF) = if (regVX(y) > regVX(x)) 1 else 0
        regVX(x) = regVX(y) - regVX(x)

      case v if (v & 0xF00F) == 0x800E =>
        // Set Vx = Vx SHL 1
        val (x, y) = getXandY(v)
        regVX(0xF) = if ((regVX(y) & 0x80) == 0x80) 1 else 0
        regVX(x) = regVX(y) << 1

      case v if (v & 0xF00F) == 0x9000 =>
        // Skip next instruction if Vx != Vy
        val (x, y) = getXandY(v)
        if (regVX(x) != regVX(y)) regPC += 2

      case v if (v & 0xF000) == 0xA000 =>
        // I = nnn
        regI = getNNN(v)

      case v if (v & 0xF000) == 0xB000 =>
        // Jump to location nnn + V0
        regPC = getNNN(v) + regVX(0)

      case v if (v & 0xF000) == 0xC000 =>
        // Set Vx = random byte AND kk
        val (x, kk) = getXandKK(v)
        regVX(x) = generateRandom() & kk

      case v if (v & 0xF000) == 0xD000 =>
        // Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.
        val (x, y, n) = getXYN(v)
        regVX(0xF) = if(memory.showSprite(n, regI, regVX(x), regVX(y))) 1 else 0

      case _ =>
        println("Not implemented! " + instr)
    }
  }
}