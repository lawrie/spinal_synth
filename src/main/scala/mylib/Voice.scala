package mylib

import spinal.core._
import spinal.lib._

class Voice(outputBits: Int = 12, freqBits: Int = 16, 
            pulseWidthBits: Int = 12, accumulatorBits: Int = 24) extends Component {

  val io = new Bundle {
    val toneFreq = in UInt(freqBits bits)
    val pulseWidth = in UInt(pulseWidthBits bits)
    val sampleClk = in Bool
    val dout = out SInt(outputBits bits)
    val waveFormEnable = in Bits(4 bits)
    val gate = in Bool
    val attack = in UInt(4 bits)
    val decay = in UInt(4 bits)
    val sustain = in UInt(4 bits)
    val release = in UInt(4 bits)
  }

  val envelope = new Envelope()
  envelope.io.a := io.attack
  envelope.io.d := io.decay
  envelope.io.s := io.sustain
  envelope.io.r := io.release
  envelope.io.gate := io.gate

  val toneGenerator = new ToneGenerator(
    accumulatorBits = accumulatorBits, 
    pulseWidthBits = pulseWidthBits, 
    outputBits = outputBits,
    freqBits = freqBits)

  toneGenerator.io.toneFreq := io.toneFreq
  toneGenerator.io.pulseWidth := io.pulseWidth
  toneGenerator.io.sampleClk := io.sampleClk
  toneGenerator.io.enTriangle := io.gate & io.waveFormEnable(0)
  toneGenerator.io.enSaw := io.gate & io.waveFormEnable(1)
  toneGenerator.io.enPulse := io.gate & io.waveFormEnable(2)
  toneGenerator.io.enNoise := io.gate & io.waveFormEnable(3)

  val amplitudeModulator = new AmplitudeModulator()
  amplitudeModulator.io.din := toneGenerator.io.dout
  amplitudeModulator.io.amplitude := envelope.io.amplitude

  io.dout := amplitudeModulator.io.dout
}

