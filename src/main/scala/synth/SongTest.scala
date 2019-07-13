package synth

import spinal.core._
import spinal.lib._

class SongTest extends PlayerComponent {
  // Song parameters
  val dataBits = 12
  val freqBits = 16

  val numRowsPerBar = 16
  val numBars = 8
  val numChannels = 4
  val numPatterns = 10
  val songLength = 24

  val compiler = new SongCompiler

  // The bars played by specific instruments
  val bar0 = compiler.emptyBar
  var bar1 = List("C2","","C2","","D2","","C2","","D#2","","C2","","F2","","D#2","")
  var bar2 = List("F2","","F2","","G2","","F2","","G#2","","F2","","A#2","","G#2","")
  var bar3 = List("G2","","G2","","A2","","G2","","A#2","","G2","","C3","","A#2","")
  val bar4 = List("C3","","","","","","","","C3","","","","","","", "")
  val bar5 = List("","","A6","","","","A6","","","","A6","","","","A6","")
  var bar6 = List("","","","","C4","","","","","","","","C4","","","")
  var bar7 = List("","","","","C4","","","","","","","","C4","","","C4")
  val bars = List(bar0, bar1, bar2, bar3, bar4, bar5, bar6, bar7)
  
  // Check and print the bars
  for (bar <- bars) {
    compiler.checkLength(bar,numRowsPerBar)
    compiler.checkBar(bar)
    compiler.printBar(bar)
  }

  // Create a rom for the bars
  val ubars = bars.map(compiler.convertBar).map(compiler.convertToUInt).flatten
  val barRom = Mem(UInt(8 bits), ubars)

  // The patterns of which bars are played by which instrument
  val patterns = List(
    List(1, 0, 0, 0),
    List(2, 0, 0, 0),         
    List(3, 0, 0, 0),        
    List(1, 4, 5, 6),
    List(2, 4, 5, 6),
    List(3, 4, 5, 6),
    List(1, 4, 5, 7),
    List(2, 4, 5, 7),
    List(3, 4, 5, 7),
    List(0, 0, 0, 0)
  ) 
  
  // Create a row for the patterns
  var patternRom = Mem(UInt(8 bits), patterns.map(compiler.convertToUInt).flatten)

  // Create a rom for the note frequencies
  val notesRom = Mem(UInt(freqBits bits), compiler.notes)

  // The song is a cyclic sequence of patterns
  val song = List(0, 0, 0, 0, 1, 1, 0, 0, 2, 1, 0, 0, 3, 6, 3, 6, 4, 7, 3, 6, 5, 7, 3, 6)

  // Create a rom for the song
  val songRom = Mem(UInt(8 bits), song.map(U(_, 8 bits)))

  // Each instrument has a gate to specify when it is played
  val instrumentGate = Reg(Bits(numChannels bits))
  val instrumentFreq = Reg(Vec(UInt(freqBits bits), numChannels))

  // Create the instruments
  val bass = Instruments.bass(dataBits)
  bass.io.sampleClk := io.sampleClk
  bass.io.gate := instrumentGate(0)
  bass.io.toneFreq := instrumentFreq(0)

  val kickDrum1 = Instruments.kickDrum1(dataBits)
  kickDrum1.io.sampleClk := io.sampleClk
  kickDrum1.io.gate := instrumentGate(1)

  val kickDrum2 = Instruments.kickDrum2(dataBits)
  kickDrum2.io.sampleClk := io.sampleClk
  kickDrum2.io.gate := instrumentGate(1)

  val kickMixer = new Mixer(dataBits = dataBits, numChannels = 2, activeChannels = 2)
  kickMixer.io.channel(0) := kickDrum1.io.dout
  kickMixer.io.channel(1) := kickDrum2.io.dout

  val highHat = Instruments.highHat(dataBits)
  highHat.io.sampleClk := io.sampleClk
  highHat.io.gate := instrumentGate(2)

  val snare = Instruments.snare(dataBits)
  snare.io.sampleClk := io.sampleClk
  snare.io.gate := instrumentGate(3)

  val filter = new FilterEwma(dataBits = dataBits)
  filter.io.sAlpha := 20
  filter.io.din := bass.io.dout

  val mixer = new Mixer(dataBits = dataBits, numChannels = 4, activeChannels = 2)
  mixer.io.channel(0) := bass.io.dout
  mixer.io.channel(1) := kickMixer.io.dout
  mixer.io.channel(2) := highHat.io.dout
  mixer.io.channel(3) := snare.io.dout

  // Flanger
  val flanger = new Flanger(sampleBits = dataBits)
  flanger.io.sampleClk := io.sampleClk
  flanger.io.din := mixer.io.dout

  // Final mix
  io.dout := mixer.io.dout 

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
      instrumentFreq(i) := ((currentNote(i) =/= 0) ? toneFreq(i) | 0) addTag(crossClockDomain)
      instrumentGate(i) := (gate && currentNote(i) =/= 0) addTag(crossClockDomain)
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
  
object SongTest {
  def main(args: Array[String]) {
    SpinalVerilog(new PdmPlayer[SongTest]())
  }
}

