package synth

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.StringBuilder

class SongCompiler {
  val noteChars = "CDEFGAB"
  
  val notes = List(U(0), U(17557), U(18601), U(19709), U(20897), U(22121), U(23436), U(24830),
                   U(26306), U(27871),U(29528), U(31234), U(33144))

  val emptyBar = List.fill(16)("")

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

        if (octaveChar < '0' || octaveChar > '6') {
          println("Invalid octave")
        } 
      }
    } 
  }

  def convertNote(note: String): Int = {
    if (note.length == 0) 0x00
    else {
      val octaveIndex = if (note.charAt(1) == '#') 2 else 1
      val octaveChar = note.charAt(octaveIndex)
      val noteIndex = noteChars.indexOf(note.charAt(0))
      val nibble1 = (noteIndex * 2) - (if (noteIndex < 3) 0 else 1) + (if (note.charAt(1) == '#') 2 else 1)
      val nibble2 = note.charAt(octaveIndex) - '0'

      ((nibble1 << 4) | nibble2)
    }
  }

  def checkBar(bar: List[String]) = bar.foreach(checkNote)

  def convertBar(bar: List[String]): List[Int] = bar.map(convertNote)

  def convertBytesToHex(bytes: List[Int]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
        sb.append(String.format("%02x ", Int.box(b)))
    }
    sb.toString
  }

  def convertToUInt(bar: List[Int]): List[UInt] = bar.map(U(_, 8 bits))

  def printBar(bar: List[String]) = {
    println(convertBytesToHex(convertBar(bar)))
  }

  def checkLength(bar: List[String], reqLength: Int = 16) = {
    if (bar.length != reqLength) println("Wrong bar length")
  }

  def convertPatterns(patternBytes: Array[BigInt], numChannels: Int): List[List[Int]] = {
    val result = new ListBuffer[List[Int]]
    for (i <- 0 until patternBytes.length / numChannels) {
      val buffer = new ListBuffer[Int]
      for(j <- 0 until numChannels) {
        buffer.append(patternBytes(i * numChannels +j).toInt)
      }
      result.append(buffer.toList)
    }
    result.toList
  }

  val noteString = Array("", "C", "C#", "D", "D#", "E", "F", "G", "G#", "A", "A#", "B")
  val octaveString = "0123456"

  def convertBarBytes(barBytes: Array[BigInt], numRowsPerBar: Int) : List[List[String]] = {
    val result = new ListBuffer[List[String]]
    for (i <- 0 until barBytes.length / numRowsPerBar) {
      val buffer = new ListBuffer[String]
      for(j <- 0 until numRowsPerBar) {
        val sb = new StringBuilder
        val b = barBytes(i * numRowsPerBar +j).toInt
        if (b != 0) {
          sb.append(noteString(b >> 4))
          sb.append(octaveString.charAt((b & 0xf)))
        }
        buffer.append(sb.toString())
      }
      result.append(buffer.toList)
    }
    result.toList
  }
}

