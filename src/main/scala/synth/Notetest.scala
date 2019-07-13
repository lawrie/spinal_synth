package synth

import spinal.core._
import spinal.lib._

class NoteTest extends PlayerComponent {
  def voice(freq: Int): Voice = {
    val voice = new Voice(outputBits = 12)
    voice.io.sampleClk := io.sampleClk
    voice.io.toneFreq := freq
    voice.io.pulseWidth :=  2047
    voice.io.waveFormEnable := B"0001"
    voice.io.attack := U"1000"
    voice.io.decay := U"0010"
    voice.io.sustain := U"1111"
    voice.io.release := U"1111"

    voice
  }
  val count = Reg(UInt(22 bits))

  count := count + 1

  val voice1 = voice(4389)
  io.dout := voice1.io.dout
  voice1.io.gate := !count.msb
  io.diag := voice1.io.diag
}

object NoteTest {
  def main(args: Array[String]) {
    SpinalVerilog(new PdmPlayer[NoteTest]())
  }
}
