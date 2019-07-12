package synth

import spinal.core._
import spinal.lib._

class SongTest extends PlayerComponent {
  // Song parameters
  val dataBits = 12
  val freqBits = 16

  val numRowsPerBar = 16
  val numBars = 4
  val numChannels = 4
  val numPatterns = 3
  val songLength = 12

  val compiler = new SongCompiler

  // The bars played by specific instruments
  val bar0 = compiler.emptyBar
  var bar1 = List("C2","","C2","","D2","","C2","","D#2","","C2","","F2","","D#2","")
  var bar2 = List("F2","","F2","","G2","","F2","","G#2","","F2","","A#2","","G#2","")
  var bar3 = List("G2","","G2","","A2","","G2","","A#2","","G2","","C3","","A#2","")
  val bars = List(bar0, bar1, bar2, bar3)
  
  // Check and print the bars
  bars.foreach {
    compiler.checkBar(_)
    compiler.printBar(_)
  }

  // Create a rom for the bars
  val ubars = bars.map(compiler.convertBar).map(compiler.convertToUInt).flatten
  val barRom = Mem(UInt(8 bits), ubars)

  // The patterns of which bars are played by which instrument
  val pattern0 = List(1, 0, 0, 0)
  val pattern1 = List(2, 0, 0, 0)
  val pattern2 = List(3, 0, 0, 0)
  val patterns = List(pattern0, pattern1, pattern2)

  // Create a row for the patterns
  var patternRom = Mem(UInt(8 bits), patterns.map(compiler.convertToUInt).flatten)

  // Create a rom for the note frequencies
  val notesRom = Mem(UInt(freqBits bits), compiler.notes)

  // The song is a cyclic sequence of patterns
  val song = List(0, 0, 0, 0, 1, 1, 0, 0, 2, 1, 0, 0)

  // Create a rom for the song
  val songRom = Mem(UInt(8 bits), song.map(U(_, 8 bits)))

  // Each instrument has a gate to specify when it is played
  val instrumentGate = Reg(Bits(numChannels bits))

  // Create the instruments
  val bass = Instruments.bass(dataBits)
  bass.io.sampleClk := io.sampleClk
  bass.io.gate := instrumentGate(0)

  // Only one instrument for this song
  io.dout := bass.io.dout 

  // Play the song in a slow tick domain
  val tickDomain = new ClockDomain(
    clock=io.tickClk,
    config=ClockDomainConfig(resetKind=BOOT)
  )

  val tickArea = new ClockingArea(tickDomain) {
    val toneFreq = Reg(UInt(freqBits bits))
    val tickTimer = Reg(UInt(3 bits)) init 0
    val songPosition = Reg(UInt(log2Up(songLength) bits)) init 0
    val barPosition = Reg(UInt(log2Up(numRowsPerBar) + 1 bits)) init 0
    val gate = Reg(Bool)
    val currentNote = Reg(Vec(UInt(8 bits), numChannels))
    val pattern = Reg(UInt(8 bits))
   
    // Get the current pattern from the song rom
    def currentPatternIdx(): UInt = songRom(songPosition)

    // Get the current bar for a specific instrument
    def currentBarForChannel(ch: UInt): UInt = patternRom(((pattern * numChannels) + ch).resized)

    // Get the current note for a specific instrument
    def currentNoteForChannel(ch: UInt): UInt = barRom(((currentBarForChannel(ch) * numRowsPerBar) + barPosition).resized)

    // Get the currtent tone frequency for a specific instrument
    def currentFreqForChannel(ch: UInt): UInt = {
      val note = currentNoteForChannel(ch)
      notesRom(note(7 downto 4)) |>> (5 - note(3 downto 0))
    }

    // Diagnostic leds
    io.diag := currentNote(3).asBits

    // Set the bass frequency
    bass.io.toneFreq := toneFreq addTag(crossClockDomain)

    tickTimer := tickTimer + 1

    // Specify which instruments are currently playing
    for (i <- 0 until numChannels) {
      instrumentGate(i) := (gate && currentNote(i) =/= 0) addTag(crossClockDomain) 
    }

    // Ticks are one eighth of a note
    when (tickTimer === 0) {
      // Update the bar poosition
      barPosition := barPosition + 1
      when (barPosition >= numRowsPerBar - 1) {
        // Start a new bar
        barPosition := 0
        songPosition := songPosition + 1
        when (songPosition >= songLength - 1) {
          // Start at the beginning again
          songPosition := 0
        }
      }
      
      // Save values in registers
      gate := True
      toneFreq := currentFreqForChannel(0).resized
      pattern := currentPatternIdx
      for (i <- 0 until numChannels) currentNote(i) := currentNoteForChannel(i)
    } elsewhen (tickTimer === 3) {
      // Switch off the instrument half way through the ticks
      gate := False
    }
  }
} 
  
object SongTest {
  def main(args: Array[String]) {
    SpinalVerilog(new PdmPlayer[SongTest]())
  }
}

