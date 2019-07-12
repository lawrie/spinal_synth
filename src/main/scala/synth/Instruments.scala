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

  def kickDrum(dataBits: Int = 12): Voice = {
    val voice = new Voice(outputBits = dataBits)
    voice.io.pulseWidth := 1000
    voice.io.waveFormEnable := B"0001"
    voice.io.attack := U"0000"
    voice.io.decay := U"0000"
    voice.io.sustain := U"1111"
    voice.io.release := U"0110"
    voice.io.toneFreq := 1383

    voice
  }

  def highHat(dataBits: Int = 12):Voice = {
    val voice = new Voice(outputBits = dataBits)
    voice.io.pulseWidth := 400
    voice.io.waveFormEnable := B"1000"
    voice.io.attack := U"0011"
    voice.io.decay := U"0001"
    voice.io.sustain := U"0100"
    voice.io.release := U"1000"
    voice.io.toneFreq := 43000

    voice
  }
}
