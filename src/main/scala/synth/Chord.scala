package synth

import spinal.core._
import spinal.lib._

class ChordTest extends PlayerComponent {
  def voice(freq: Int): Voice = {
    val voice = new Voice(outputBits = 12)
    voice.io.sampleClk := io.sampleClk
    voice.io.toneFreq := freq
    voice.io.pulseWidth :=  2047
    voice.io.waveFormEnable := B"0001"
    voice.io.attack := U"0010"
    voice.io.decay := U"0010"
    voice.io.sustain := U"1000"
    voice.io.release := U"1100"
    voice.io.gate := !io.gate

    voice
  }

  val voice1 = voice(4389)
  val voice2 = voice(5530)
  val voice3 = voice(6577)

  val mixer = new Mixer(dataBits = 12, numChannels = 3, activeChannels = 3)
  mixer.io.channel(0) := voice1.io.dout
  mixer.io.channel(1) := voice2.io.dout
  mixer.io.channel(2) := voice3.io.dout

  io.dout := mixer.io.dout
  io.diag := 0
}

object ChordTest {
  def main(args: Array[String]) {
    SpinalVerilog(new PdmPlayer[ChordTest]())
  }
}
