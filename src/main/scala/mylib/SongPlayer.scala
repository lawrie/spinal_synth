package mylib

import spinal.core._
import spinal.lib._

class SongPlayer(dataBits: Int = 12, freqBits: Int = 16) extends Component {
  val io = new Bundle {
    val sampleClk = in Bool
    val tickClk = in Bool
    val dout = out SInt(dataBits bits)
    val diag = out Bits(8 bits)
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
  val notesRom = Mem(UInt(freqBits bits), notes)

  val instrumentGate = Reg(Bits(numChannels bits))

  // Voices
  val bass = new Voice(outputBits = dataBits)
  bass.io.sampleClk := io.sampleClk
  bass.io.pulseWidth :=  2048
  bass.io.waveFormEnable := B"0110"
  bass.io.attack := U"0100"
  bass.io.decay := U"0010"
  bass.io.sustain := U"0100"
  bass.io.release := U"1100"
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

  val highHat = new Voice(outputBits = dataBits)
  highHat.io.sampleClk := io.sampleClk
  highHat.io.pulseWidth := 400
  highHat.io.waveFormEnable := B"1000"
  highHat.io.attack := U"0011"
  highHat.io.decay := U"0001"
  highHat.io.sustain := U"0100"
  highHat.io.release := U"1000"
  highHat.io.toneFreq := 43000
  highHat.io.gate := instrumentGate(2)

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

  // Filter
  val filter = new FilterEwma(dataBits = dataBits)
  filter.io.sAlpha := 20
  filter.io.din := bass.io.dout

  // Mixer
  val mixer = new Mixer(dataBits = dataBits, activeChannels = 5)
  mixer.io.channel(0) := filter.io.dout
  mixer.io.channel(1) := kickDrum.io.dout
  mixer.io.channel(2) := kickDrum2.io.dout
  mixer.io.channel(3) := highHat.io.dout
  mixer.io.channel(4) := snare.io.dout

  // Flanger
  val flanger = new Flanger(sampleBits = dataBits)
  flanger.io.sampleClk := io.sampleClk
  flanger.io.din := mixer.io.dout

  // Final output
  io.dout := flanger.io.dout
  
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
    
    def currentPatternIdx(): UInt = songRom(songPosition)
    def currentBarForChannel(ch: UInt): UInt = patternRom(((pattern * numChannels) + ch).resized)
    def currentNoteForChannel(ch: UInt): UInt = barRom(((currentBarForChannel(ch) * numRowsPerBar) + barPosition).resized)
    def currentFreqForChannel(ch: UInt): UInt = {
      val note = currentNoteForChannel(ch)
      notesRom(note(7 downto 4)) |>> (6 - note(3 downto 0))
    }

    //io.diag := (barPosition ## tickTimer).resized
    //io.diag := songPosition.asBits.resized
    io.diag := currentNote(3).asBits
    //io.diag := pattern.asBits

    bass.io.toneFreq := toneFreq addTag(crossClockDomain)

    tickTimer := tickTimer + 1

    for (i <- 0 until numChannels) {
      instrumentGate(i) := (gate && currentNote(i) =/= 0) addTag(crossClockDomain) 
    }

    when (tickTimer === 0) {
      barPosition := barPosition + 1
      when (barPosition >= numRowsPerBar - 1) {
        barPosition := 0
        songPosition := songPosition + 1
        when (songPosition >= songLength - 1) {
          songPosition := 0
        }
      }
      
      gate := True
      toneFreq := currentFreqForChannel(0).resized
      pattern := currentPatternIdx
      for (i <- 0 until numChannels) currentNote(i) := currentNoteForChannel(i)
    } elsewhen (tickTimer === 3) {
      gate := False
    }
  }
}

