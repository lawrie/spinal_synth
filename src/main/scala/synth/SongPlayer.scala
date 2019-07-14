package synth

import spinal.core._
import spinal.lib._

class SongPlayer(
  freqBits: Int = 16,
  numChannels : Int,
  numRowsPerBar: Int = 16,
  bars : List[List[String]],
  patterns: List[List[Int]],
  song: List[Int]
) extends Component {

  val io = new Bundle {
    val tickClk = in Bool
    val diag = out Bits(8 bits)
    val instrumentGate = out Vec(Bool, numChannels)
    val instrumentFreq = out Vec(UInt(freqBits bits), numChannels)
  }

  val compiler = new SongCompiler

  val songLength = song.length

  // Check and print the bars
  for (bar <- bars) {
    compiler.checkLength(bar,numRowsPerBar)
    compiler.checkBar(bar)
    compiler.printBar(bar)
  }

  // Create a rom for the bars
  val ubars = bars.map(compiler.convertBar).map(compiler.convertToUInt).flatten
  val barRom = Mem(UInt(8 bits), ubars)

  // Create a rom for the patterns
  var patternRom = Mem(UInt(8 bits), patterns.map(compiler.convertToUInt).flatten)

  // Create a rom for the note frequencies
  val notesRom = Mem(UInt(freqBits bits), compiler.notes)

  // Create a rom for the song
  val songRom = Mem(UInt(8 bits), song.map(U(_, 8 bits)))

  // Play the song in a slow tick domain
  val tickDomain = new ClockDomain(
    clock=io.tickClk,
    config=ClockDomainConfig(resetKind=BOOT)
  )

  val tickArea = new ClockingArea(tickDomain) {
    val toneFreq = Reg(Vec(UInt(freqBits bits), numChannels))
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
      notesRom(note(7 downto 4)) |>> (6 - note(3 downto 0))
    }
  
    // Diagnostic leds
    io.diag := currentNote(3).asBits

    // Set the bass frequency
    for (i <- 0 until numChannels) {
      io.instrumentFreq(i) := ((currentNote(i) =/= 0) ? toneFreq(i) | 0) addTag(crossClockDomain)
      io.instrumentGate(i) := (gate && currentNote(i) =/= 0) addTag(crossClockDomain)
    }

    tickTimer := tickTimer + 1

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

      for(i <- 0 until numChannels) {
        toneFreq(i) := currentFreqForChannel(i).resized
        currentNote(i) := currentNoteForChannel(i)
      }

      pattern := currentPatternIdx
    } elsewhen (tickTimer === 3) {
      // Switch off the instrument half way through the ticks
      gate := False
    }
  }
} 
