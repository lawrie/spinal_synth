package synth

import spinal.core._
import spinal.lib._

class SongTest extends PlayerComponent {
  // Song parameters
  implicit val dataBits = 12
  val freqBits = 16

  val numRowsPerBar = 16
  val numChannels = 4

  // The bars played by specific instruments
  val bars = List(
    List("",   "", "",   "", "",   "", "",   "", "",    "", "",   "", "",    "", "",    ""),  // Silent
    List("C2", "", "C2", "", "D2", "", "C2", "", "D#2", "", "C2", "", "F2",  "", "D#2", ""),  // Bass
    List("F2", "", "F2", "", "G2", "", "F2", "", "G#2", "", "F2", "", "A#2", "", "G#2", ""),  // Bass
    List("G2", "", "G2", "", "A2", "", "G2", "", "A#2", "", "G2", "", "C3",  "", "A#2", ""),  // Bass
    List("C3", "", "",   "", "",   "", "",   "", "C3",  "", "",   "", "",    "", "",    ""),  // Bass
    List("",   "", "A6", "", "",   "", "A6", "", "",    "", "A6", "", "",    "", "A6",  ""),  // Kick drum
    List("",   "", "",   "", "C4", "", "",   "", "",    "", "",   "", "C4",  "", "",    ""),  // High hat
    List("",   "", "",   "", "C4", "", "",   "", "",    "", "",   "", "C4",  "", "",    "C4") // Snare drum
  )
  
  // The patterns of which bars are played by which instrument
  val patterns = List(
    List(1, 0, 0, 0), // Bass only
    List(2, 0, 0, 0), // Bass only
    List(3, 0, 0, 0), // Bass only 
    List(1, 4, 5, 6), // Bass and drums
    List(2, 4, 5, 6), // Bass and drums
    List(3, 4, 5, 6), // Bass and drums
    List(1, 4, 5, 7), // Bass and drums
    List(2, 4, 5, 7), // Bass and drums
    List(3, 4, 5, 7), // Bass and drums
    List(0, 0, 0, 0)  // Silent
  ) 
  
  // The song is a cyclic sequence of patterns
  val song = List(0, 0, 0, 0, 1, 1, 0, 0, 2, 1, 0, 0, 3, 6, 3, 6, 4, 7, 3, 6, 5, 7, 3, 6)
  
  // Each instrument has a gate to specify when it is played and a tone frequency
  val instrumentGate = Reg(Bits(numChannels bits))
  val instrumentFreq = Reg(Vec(UInt(freqBits bits), numChannels))

  // Create the instruments
  val bass = Instruments.bass
  bass.io.gate := instrumentGate(0)
  bass.io.toneFreq := instrumentFreq(0)

  // Kick drum is two voices and a mixer
  val kickDrum1 = Instruments.kickDrum1
  kickDrum1.io.gate := instrumentGate(1)

  val kickDrum2 = Instruments.kickDrum2
  kickDrum2.io.gate := instrumentGate(1)

  val kickMixer = new Mixer(dataBits = dataBits, numChannels = 2, activeChannels = 2)
  kickMixer.io.channel(0) := kickDrum1.io.dout
  kickMixer.io.channel(1) := kickDrum2.io.dout

  val highHat = Instruments.highHat
  highHat.io.gate := instrumentGate(2)

  val snare = Instruments.snare
  snare.io.gate := instrumentGate(3)

  // Create an EWMA filter for the bass
  val filter = new FilterEwma
  filter.io.sampleClk := io.sampleClk
  filter.io.sAlpha := 20
  filter.io.din := bass.io.dout

  // Create a mixer to mix all the instruments
  val mixer = new Mixer(dataBits = dataBits, numChannels = 4, activeChannels = 2)
  mixer.io.channel(0) := filter.io.dout
  mixer.io.channel(1) := kickMixer.io.dout
  mixer.io.channel(2) := highHat.io.dout
  mixer.io.channel(3) := snare.io.dout

  // Create a flanger
  val flanger = new Flanger(sampleBits = dataBits)
  flanger.io.sampleClk := io.sampleClk
  flanger.io.din := mixer.io.dout

  // Final mix
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
  
object SongTest {
  def main(args: Array[String]) {
    SpinalVerilog(new PdmPlayer[SongTest]())
  }
}

