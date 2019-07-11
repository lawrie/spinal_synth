package mylib

import spinal.core._
import spinal.lib._

class SongCompiler {
  val noteChars = "CDEFGAB"
 
  def checkNote(note: String) = {
    if (note.length > 0) {
      if (note.length == 1 || note.length > 3) {
        print("Invalid note length")
      } else {
        val noteChar = note.charAt(0)

        if (noteChars.indexOf(noteChar) < 0) {
          println("invalid note")
        }

        if (note.charAt(1) == '#' && (noteChar == 'B' || noteChar == 'E')) {
          println("Invalid sharp")
        }

        val octaveIndex = if (note.charAt(1) == '#') 2 else 1
        val octaveChar = note.charAt(octaveIndex)

        if (octaveChar >= '0' && octaveChar >= '6') {
          println("Invalid octave")
        } 
      }
    } 
  }

  def convertNote(note: String): Byte = {
    if (note.length == 0) 0x00
    else {
      val octaveIndex = if (note.charAt(1) == '#') 2 else 1
      val octaveChar = note.charAt(octaveIndex)
      val noteIndex = noteChars.indexOf(note.charAt(0))
      val nibble1 = (noteIndex * 2) - (if (noteIndex < 3) 0 else 1) + (if (note.charAt(1) == '#') 2 else 1)
      val nibble2 = note.charAt(octaveIndex) - '0'

      ((nibble1 << 4) | nibble2).toByte
    }
  }

  def checkBar(bar: List[String]) = bar.foreach(checkNote)

  def convertBar(bar: List[String]): List[Byte] = bar.map(convertNote)

  def convertBytesToHex(bytes: List[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
        sb.append(String.format("%02x ", Byte.box(b)))
    }
    sb.toString
  }

  def printBar(bar: List[String]) = {
    println(convertBytesToHex(convertBar(bar)))
  }
}

object SongTest {
  val bar0 = List.fill(16)("")
  var bar1 = List("C2","","C2","","D2","","C2","","D#2","","C2","","F2","","D#2","")
  var bar2 = List("F2","","F2","","G2","","F2","","G#2","","F2","","A#2","","G#2","")
  var bar3 = List("G2","","G2","","A2","","G2","","A#2","","G2","","C3","","A#2","")

  val pattern0 = List(1,0,0,0)
  val pattern1 = List(2,0,0,0)
  val pattern2 = List(3,0,0,0)

  val song = List(pattern0, pattern1, pattern2)

  def main(args: Array[String]) {
    val compiler = new SongCompiler
    List(bar0, bar1, bar2, bar3).foreach {
      compiler.checkBar(_)
      compiler.printBar(_)
    }
  }
}

