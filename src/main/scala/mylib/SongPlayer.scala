package mylib

import spinal.core._
import spinal.lib._

class SongPlayer(dataBits: Int = 12) extends Component {
  val io = new Bundle {
    val sampleClk = in Bool
    val tickClk = in Bool
    val dout = out SInt(dataBits bits)
  }

  // Song parameters
  val numRowsPerBar = 16
  val numBars = 8
  val numChannels = 4
  val numPatterns = 10
  val songLength = 24
  
  val barRom = Mem(UInt(8 bits), wordCount=numBars*numRowsPerBar)
  barRom.initialContent = Tools.readmemh("example_song_bars.rom")

  val patternRom = Mem(UInt(8 bits), wordCount=numPatterns*numChannels)
  patternRom.initialContent = Tools.readmemh("example_song_patterns.rom")

  val songRom = Mem(UInt(8 bits), wordCount=songLength)
  songRom.initialContent = Tools.readmemh("example_song_pattern_map.rom")

  val notes = Array(U(0), U(17557), U(18601), U(19709), U(20897), U(22121), U(23436), U(24830),
                   U(26306), U(27871),U(29528), U(31234), U(33144))
  val notesRom = Mem(UInt(16 bits), notes)

  //val tune = Array(U(1), U(1), U(8), U(8), U(10), U(10), U(8), U(0))
  //val tuneRom = Mem(UInt(4 bits), tune)
  
  //val instrumentFrequency = Reg(Vec(UInt(16 bits), 4))
  
  val instrumentGate = Reg(Bits(4 bits))

  // Voices
  val bass = new Voice(outputBits = dataBits)
  bass.io.sampleClk := io.sampleClk
  bass.io.pulseWidth := 2048
  bass.io.waveFormEnable := B"0110"
  bass.io.attack := U"0011"
  bass.io.decay := U"0000"
  bass.io.sustain := U"1111"
  bass.io.release := U"0110"
  bass.io.gate := instrumentGate(0)

  val kickDrum = new Voice(outputBits = dataBits)
  kickDrum.io.sampleClk := io.sampleClk
  kickDrum.io.pulseWidth := 1000
  kickDrum.io.waveFormEnable := B"0001"
  kickDrum.io.attack := U"0000"
  kickDrum.io.decay := U"0000"
  kickDrum.io.sustain := U"1111"
  kickDrum.io.release := U"0110"
  kickDrum.io.toneFreq := 1383
  kickDrum.io.gate := instrumentGate(1)
  //kickDrum.io.gate := False

  val kickDrum2 = new Voice(outputBits = dataBits)
  kickDrum2.io.sampleClk := io.sampleClk
  kickDrum2.io.pulseWidth := 1000
  kickDrum2.io.waveFormEnable := B"1000"
  kickDrum2.io.attack := U"0000"
  kickDrum2.io.decay := U"0000"
  kickDrum2.io.sustain := U"0010"
  kickDrum2.io.release := U"0000"
  kickDrum2.io.toneFreq := 18000
  kickDrum2.io.gate := instrumentGate(1)
  //kickDrum2.io.gate := False

  val highHat = new Voice(outputBits = dataBits)
  highHat.io.sampleClk := io.sampleClk
  highHat.io.pulseWidth := 400
  highHat.io.waveFormEnable := B"1000"
  highHat.io.attack := U"0011"
  highHat.io.decay := U"0001"
  highHat.io.sustain := U"0100"
  highHat.io.release := U"1000"
  highHat.io.toneFreq := 3000
  highHat.io.gate := instrumentGate(2)
  //highHat.io.gate := False

  val snare = new Voice(outputBits = dataBits)
  snare.io.sampleClk := io.sampleClk
  snare.io.pulseWidth := 400
  snare.io.waveFormEnable := B"1000"
  snare.io.attack := U"0010"
  snare.io.decay := U"0010"
  snare.io.sustain := U"0001"
  snare.io.release := U"0100"
  snare.io.toneFreq := 2000
  snare.io.gate := instrumentGate(3)
  //snare.io.gate := False

  // Mixer
  val mixer = new MultiChannelMixer(dataBits = dataBits, activeChannels = 5)
  mixer.io.a := bass.io.dout
  mixer.io.b := kickDrum.io.dout
  mixer.io.c := kickDrum2.io.dout
  mixer.io.d := highHat.io.dout
  mixer.io.e := snare.io.dout
  mixer.io.f := 0
  mixer.io.g := 0
  mixer.io.h := 0
  mixer.io.i := 0
  mixer.io.j := 0
  mixer.io.k := 0
  mixer.io.l := 0

  // Filter
  val filter = new FilterEwma(dataBits = dataBits)
  filter.io.sAlpha := 20
  filter.io.din := mixer.io.dout
  io.dout := filter.io.dout

  // Play the song in a slow tick domain
  val tickDomain = new ClockDomain(clock=io.tickClk) 

  val tickArea = new ClockingArea(tickDomain) {
    val toneFreq = Reg(UInt(16 bits))
    val tickTimer = Reg(UInt(2 bits))
    //val noteCounter = Reg(UInt(3 bits)) 
    val songPosition = Reg(UInt(8 bits)) 
    val barPosition = Reg(UInt(8 bits)) 
    val gate = Reg(Bool)
    val currentNote = Reg(UInt(8 bits))
    
    def currentPatternIdx(): UInt = songRom(songPosition.resized)
    def currentBarForChannel(ch: UInt): UInt = patternRom(((currentPatternIdx() * numChannels) + ch).resized)
    def currentNoteForChannel(ch: UInt): UInt = barRom(((currentBarForChannel(ch) * numRowsPerBar) + barPosition).resized)
    def currentFreqForChannel(ch: UInt): UInt = {
      val note = currentNoteForChannel(ch)
      notesRom((note >> 4) & 0xf) >> (6 - (note & 0xf))
    }

    bass.io.toneFreq := toneFreq addTag(crossClockDomain)

    tickTimer := tickTimer + 1

    for (i <- 0 until 4) {
      instrumentGate(i) := gate addTag(crossClockDomain) 
    }

    when (tickTimer === 0) {
      barPosition := barPosition + 1
      when (barPosition >= numRowsPerBar) {
        barPosition := 0
        songPosition := songPosition + 1
        when (songPosition >= songLength) {
          songPosition := 0
        }
      }
      
      gate := True
      //toneFreq := notesRom(tuneRom(noteCounter))
      toneFreq := currentFreqForChannel(0).resized
      currentNote := currentNoteForChannel(0)
      //noteCounter := noteCounter + 1
    } elsewhen (tickTimer === 3) {
      gate := False
    }
  }
}

