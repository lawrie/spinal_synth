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

class ToneGeneratorNoise(accumulatorBits: Int = 24, outputBits: Int = 12) extends Component {
  val io = new Bundle {
    val accumulator = in UInt(accumulatorBits bits)
    val dout = out UInt(outputBits bits)
  }

  val lfsr = Reg(Bits(23 bits)) init B"01101110010010000101011"

  lfsr := lfsr(21 downto 0) ## (lfsr(22) ^ lfsr(17))

  io.dout := (lfsr(22) ## lfsr(20) ## lfsr(16) ## lfsr(13) ## lfsr(11) ## lfsr(7) ## lfsr(4) ## lfsr(2) ## B(0, outputBits - 8 bits)).asUInt

}

class ToneGenerator(accumulatorBits: Int = 24, pulseWidthBits: Int = 12, outputBits: Int = 12, freqBits: Int = 16) extends Component {
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

class ToneTest(outputBits: Int = 12) extends Component {
  val io = new Bundle {
    val audio = out Bool
    val leds = out Bits(8 bits)
    val switches = in Bits(4 bits)
    val quadA = in Bool
    val quadB = in Bool
  }

  val clockHz = 100000000

  val oneMHzClk = new ClkDivider(clockHz / 1000000)
  val sampleClk = new ClkDivider(clockHz / 44100)

  val oneMHzDomain = new ClockDomain(clock=oneMHzClk.io.cout, reset=clockDomain.reset)
  val pdm = new Pdm(outputBits)

  val oneMHzArea = new ClockingArea(oneMHzDomain) {
    val quad = new Quad(16)
    quad.io.initValue := 1000
    quad.io.quadA := io.quadA
    quad.io.quadB := io.quadB
    
    val toneGenerator = new ToneGenerator(outputBits = outputBits)
    toneGenerator.io.sampleClk := sampleClk.io.cout
    toneGenerator.io.pulseWidth := 1000
    toneGenerator.io.toneFreq := quad.io.position
    toneGenerator.io.enPulse := io.switches(0)
    toneGenerator.io.enNoise := io.switches(1)
    toneGenerator.io.enSaw := io.switches(2)
    toneGenerator.io.enTriangle := io.switches(3)

    pdm.io.din := toneGenerator.io.dout addTag(crossClockDomain)
    io.leds := toneGenerator.io.dout.asBits.resized
  }

  io.audio := pdm.io.dout
}

object ToneTest {
  def main(args: Array[String]) {
    SpinalVerilog(new ToneTest(12))
  }
}

object ToneSim {
  import spinal.core.sim._

  def main(args: Array[String]) {
    SimConfig.withWave.compile(new ToneTest(12)).doSim{ dut =>
      dut.clockDomain.forkStimulus(100)

      dut.clockDomain.waitSampling(100000)
    }
  }
}

