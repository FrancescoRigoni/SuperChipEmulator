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
    'A' -> 10,
    'B' -> 11,
    'C' -> 12,
    'D' -> 13,
    'E' -> 14,
    'F' -> 15,
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

