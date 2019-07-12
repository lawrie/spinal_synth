package synth

import spinal.core._
import spinal.lib._

object Instruments {
  def bass(dataBits: Int = 12): Voice = {
    val voice = new Voice(outputBits = dataBits)
    voice.io.pulseWidth :=  2048
    voice.io.waveFormEnable := B"0110"
    voice.io.attack := U"0100"
    voice.io.decay := U"0010"
    voice.io.sustain := U"0100"
    voice.io.release := U"1100"

    voice
  }
}
