package mylib

import spinal.core._
import spinal.lib._

class ToneGeneratorPulse(accumulatorBits: Int = 24, pulseWidthBits: Int = 12, outputBits: Int = 12) extends Component {
  val io = new Bundle {
    val accumulator = in UInt(accumulatorBits bits)
    val pulseWidth = in UInt(pulseWidthBits bits)
    val dout = out UInt(outputBits bits)
  }

  val maxScale = (1 << outputBits) - 1

  io.dout := (io.accumulator(accumulatorBits - 1 downto accumulatorBits - pulseWidthBits) <= io.pulseWidth) ? U(maxScale) | U(0)
}

class ToneGeneratorTriangle(accumulatorBits: Int = 24, outputBits: Int = 12) extends Component {
  val io = new Bundle {
    val accumulator = in UInt(accumulatorBits bits)
    val dout = out UInt(outputBits bits)
  }

  val invert = io.accumulator.msb
  val bits = io.accumulator(accumulatorBits - 2 downto accumulatorBits - 1 - outputBits)

  io.dout := invert ? ~bits | bits
}

class ToneGeneratorSaw(accumulatorBits: Int = 24, outputBits: Int = 12) extends Component {
  val io = new Bundle {
    val accumulator = in UInt(accumulatorBits bits)
    val dout = out UInt(outputBits bits)
  }

  io.dout := io.accumulator(accumulatorBits - 1 downto accumulatorBits - outputBits)

}

class ToneGeneratorNoise(outputBits: Int = 12) extends Component {
  val io = new Bundle {
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
  //val prevAccumulator = Reg(UInt(accumulatorBits bits))

  //prevAccumulator := accumulator
  accumulator := accumulator + io.toneFreq

  val pulse = new ToneGeneratorPulse(accumulatorBits, pulseWidthBits, outputBits)
  pulse.io.accumulator := accumulator
  pulse.io.pulseWidth := io.pulseWidth

  val triangle = new ToneGeneratorTriangle(accumulatorBits, outputBits)
  triangle.io.accumulator := accumulator

  val saw = new ToneGeneratorSaw(accumulatorBits, outputBits)
  saw.io.accumulator := accumulator

  val noise = new ToneGeneratorNoise(outputBits)

  val sampleDomain = ClockDomain( clock=io.sampleClk)

  val sampleArea = new ClockingArea(sampleDomain) {
    val doutReg = Reg(SInt(outputBits bits))
    val doutTemp1 = Reg(UInt(outputBits bits))
    doutTemp1 := (io.enNoise ? noise.io.dout | U((1 << outputBits) - 1)) addTag(crossClockDomain)
    val doutTemp2 = (io.enSaw ? (saw.io.dout & doutTemp1) | doutTemp1)
    val doutTemp3 = (io.enTriangle ? (triangle.io.dout & doutTemp2) | doutTemp2)
    val doutTemp4 = (io.enPulse ? (pulse.io.dout & doutTemp3) | doutTemp3) ^ U((1 << (outputBits -1)))

    doutReg := doutTemp4.asSInt

    io.dout := doutReg addTag(crossClockDomain)
  }
}

class ToneTest extends PlayerComponent {
  val quad = new Quad(12)
  quad.io.initValue := 1000
  quad.io.quadA := io.quadA
  quad.io.quadB := io.quadB

  val toneGenerator = new ToneGenerator(outputBits = 12)
  toneGenerator.io.sampleClk := io.sampleClk
  toneGenerator.io.pulseWidth := quad.io.position
  toneGenerator.io.toneFreq := 1000
  toneGenerator.io.enPulse := io.switches(0)
  toneGenerator.io.enNoise := io.switches(1)
  toneGenerator.io.enSaw := io.switches(2)
  toneGenerator.io.enTriangle := io.switches(3)

  io.diag := 0
  io.dout := toneGenerator.io.dout
}

object ToneTest {
  def main(args: Array[String]) {
    SpinalVerilog(new PdmPlayer[ToneTest]())
  }
}

object ToneSim {
  import spinal.core.sim._

  def main(args: Array[String]) {
    SimConfig.withWave.compile(new PdmPlayer[ToneTest]()).doSim{ dut =>
      dut.clockDomain.forkStimulus(100)

      dut.clockDomain.waitSampling(100000)
    }
  }
}

