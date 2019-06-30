package mylib

import spinal.core._
import spinal.lib._

class ToneGeneratorPulse(accumulatorBits: Int, pulseWidthBits: Int, outputBits: Int) extends Component {
  val io = new Bundle {
    val accumulator = in UInt(accumulatorBits bits)
    val pulseWidth = in UInt(pulseWidthBits bits)
    val dout = out UInt(outputBits bits)
  }

  val maxScale = (1 << outputBits) - 1

  io.dout := (io.accumulator(accumulatorBits - 1 downto accumulatorBits - pulseWidthBits) < io.pulseWidth) ? U(maxScale) | U(0)
}

class ToneGeneratorTriangle(accumulatorBits: Int, outputBits: Int) extends Component {
  val io = new Bundle {
    val accumulator = in UInt(accumulatorBits bits)
    val dout = out UInt(outputBits bits)
  }

  val invert = io.accumulator.msb
  val bits = io.accumulator(accumulatorBits - 2 downto accumulatorBits - 1 - outputBits)

  io.dout := invert ? ~bits | bits
}

class ToneGeneratorSaw(accumulatorBits: Int, outputBits: Int) extends Component {
  val io = new Bundle {
    val accumulator = in UInt(accumulatorBits bits)
    val dout = out UInt(outputBits bits)
  }

  io.dout := io.accumulator(accumulatorBits - 1 downto accumulatorBits - outputBits)

}

class ToneGeneratorNoise(accumulatorBits: Int, outputBits: Int) extends Component {
  val io = new Bundle {
    val accumulator = in UInt(accumulatorBits bits)
    val dout = out UInt(outputBits bits)
  }

  val lfsr = Reg(Bits(23 bits)) init B"01101110010010000101011"

  lfsr := lfsr(21 downto 0) ## (lfsr(22) ^ lfsr(17))

  io.dout := (lfsr(22) ## lfsr(20) ## lfsr(16) ## lfsr(13) ## lfsr(11) ## lfsr(7) ## lfsr(4) ## lfsr(2) ## B(0, outputBits bits)).asUInt.resized

}

class ToneGenerator(accumulatorBits: Int, pulseWidthBits: Int, outputBits: Int, freqBits: Int) extends Component {
  val io = new Bundle {
    val sampleClk = in Bool
    val pulseWidth = in UInt(pulseWidthBits bits)
    val toneFreq = in UInt(freqBits bits)
    val dout = out SInt(outputBits bits)
    val enPulse = in Bool
    val enTriangle = in Bool
    val enSaw = in Bool
    val enNoise = in Bool
  }

  val accumulator = Reg(UInt(accumulatorBits bits))
  val prevAccumulator = Reg(UInt(accumulatorBits bits))

  prevAccumulator := accumulator
  accumulator := accumulator + io.toneFreq

  val pulse = new ToneGeneratorPulse(accumulatorBits, pulseWidthBits, outputBits)
  pulse.io.accumulator := accumulator
  pulse.io.pulseWidth := io.pulseWidth

  val triangle = new ToneGeneratorTriangle(accumulatorBits, outputBits)
  triangle.io.accumulator := accumulator

  val saw = new ToneGeneratorSaw(accumulatorBits, outputBits)
  saw.io.accumulator := accumulator

  val noise = new ToneGeneratorNoise(accumulatorBits, outputBits)
  noise.io.accumulator := accumulator

  val sampleDomain = ClockDomain( clock=io.sampleClk)

  val sampleArea = new ClockingArea(sampleDomain) {
    val doutTemp = UInt(outputBits bits)
    val doutReg = Reg(UInt(outputBits bits))
    doutReg := doutTemp addTag(crossClockDomain)

    doutTemp := (1 << outputBits) - 1
    
    when (io.enPulse) {
      doutTemp := pulse.io.dout ^ (1 << (outputBits - 1)) addTag(crossClockDomain)
    }

    when (io.enTriangle) {
      doutTemp := triangle.io.dout ^ (1 << (outputBits - 1)) addTag(crossClockDomain)
    }

    when (io.enSaw) {
      doutTemp := saw.io.dout ^ (1 << (outputBits - 1)) addTag(crossClockDomain)
    }

    when (io.enNoise) {
      doutTemp := noise.io.dout ^ (1 << (outputBits - 1)) addTag(crossClockDomain)
    }

    io.dout := doutTemp.asSInt addTag(crossClockDomain)
  }
}

