package synth

import spinal.core._
import spinal.lib._

class SongExample() extends PlayerComponent {
  implicit val dataBits = 12
  val freqBits = 16
  
  // Song parameters
  val numRowsPerBar = 16
  val numChannels = 4
  val numPatterns = 10

  val compiler = new SongCompiler()

  val bars = compiler.convertBarBytes(Tools.readmemh("example_song_bars.rom"), numRowsPerBar)
  val patterns = compiler.convertPatterns(Tools.readmemh("example_song_patterns.rom"), numChannels)
  val song = Tools.readList("example_song_pattern_map.rom")

  val instrumentGate = Reg(Vec(Bool, numChannels))
  val instrumentFreq = Reg(Vec(UInt(freqBits bits), numChannels))

  // Voices
  val bass = Instruments.bass
  bass.io.gate := instrumentGate(0)
  bass.io.toneFreq := instrumentFreq(0)

  val kickDrum = Instruments.kickDrum1
  kickDrum.io.gate := instrumentGate(1)

  val kickDrum2 = Instruments.kickDrum2
  kickDrum2.io.gate := instrumentGate(1)

  val highHat = Instruments.highHat
  highHat.io.gate := instrumentGate(2)

  val snare = Instruments.snare
  snare.io.gate := instrumentGate(3)

  // Filter
  val filter = new FilterEwma
  filter.io.sAlpha := 20
  filter.io.din := bass.io.dout

  // Mixer
  val mixer = new Mixer(dataBits = dataBits, numChannels=5, activeChannels = 2)
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

  // Execute the song using the tick clock
  val songPlayer = new SongPlayer(
    freqBits = freqBits,
    numChannels = numChannels,
    numRowsPerBar = numRowsPerBar,
    bars = bars,
    patterns = patterns,
    song = song
  )
  songPlayer.io.tickClk := io.tickClk

  io.diag := songPlayer.io.diag

  for(i <- 0 until numChannels) {
    instrumentGate(i) := songPlayer.io.instrumentGate(i)
    instrumentFreq(i) := songPlayer.io.instrumentFreq(i)
  }
}

object SongExample {
  def main(args: Array[String]) {
    SpinalVerilog(new PdmPlayer[SongExample](dataBits = 12))
  }
}

