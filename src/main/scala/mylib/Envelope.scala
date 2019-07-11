package mylib

import spinal.core._
import spinal.lib._

class Envelope(accumulatorBits : Int = 26, sampleClkFreq: Int = 44100)  extends Component {
  val io = new Bundle {
    val gate = in Bool
    val a = in UInt(4 bits)
    val d = in UInt(4 bits)
    val s = in UInt(4 bits)
    val r = in UInt(4 bits)
    val amplitude = out UInt(8 bits)
  }

  val accumulator = Reg(UInt(accumulatorBits bits))
  val accumulatorInc = Reg(UInt(16 bits))

  object op extends SpinalEnum {
    val Off, Attack, Decay, Sustain, Release = newElement()
  }

  val state = Reg(op)

  val prevGate = Reg(Bool)

  val overflow = accumulator.msb
  val decTmp = Reg(UInt(20 bits))
  val relTmp = Reg(UInt(20 bits))

  val accumulatorSize = 1 << accumulatorBits
  val accumulatorMax = accumulatorSize - 1

  def calculatePhaseIncrement(n : Double): Int = {
    (accumulatorSize.toDouble / (n * sampleClkFreq.toDouble)).toInt
  }

  val attackIncs = Array(0.002, 0.008, 0.016, 0.024, 0.038, 0.056, 0.068, 0.080,
                         0.1, 0.25, 0.5, 0.8, 1.0, 3.0, 5.0, 8.0)

  val decayIncs = Array(0.006, 0.024, 0.048, 0.072, 0.114, 0.168, 0.204,
                        0.24, 0.3, 0.75, 1,5, 2.4, 3.0, 9.0, 15.0, 24.0)

  def attackTable = for(i <- 0 until 16) yield {
    U(calculatePhaseIncrement(attackIncs(i)))
  }

  def decayTable = for(i <- 0 until 16) yield {
    U(calculatePhaseIncrement(decayIncs(i)))
  }

  val attackRom = Mem(UInt(20 bits), attackTable)
  val decayRom = Mem(UInt(20 bits), decayTable)

  val expDecayRom = Mem(UInt(8 bits), wordCount = 256)
  expDecayRom.initialContent = Tools.readmemh("exp_lookup_table.rom")

  val expOut = expDecayRom(accumulator(accumulatorBits -1 downto accumulatorBits - 8))

  val amplitude = Reg(UInt(8 bits))
  io.amplitude := amplitude

  val sustainVolume = io.s @@ U"0000"

  val nextState = Reg(op)

  prevGate := io.gate

  switch(state) {
    is(op.Attack) {
      nextState := io.gate ? op.Decay | op.Release
    }
    is(op.Decay) {
      nextState := io.gate ? op.Sustain | op.Release
    }
    is (op.Sustain) {
      nextState := io.gate ? op.Sustain | op.Release
    }
    is (op.Release) {
      nextState := io.gate ? op.Attack | op.Off
    }
    is (op.Off) {
      nextState := io.gate ? op.Attack | op.Off
    }
  }

  when (io.gate & !prevGate) {
    accumulator := 0
    state := op.Attack
  }

  when (overflow) {
    accumulator := 0;
    decTmp := U(255).resized
    state := nextState
  } otherwise {
    switch(state) {
      is(op.Attack) {
        accumulator := accumulator + attackRom(io.a)
        amplitude := accumulator(accumulatorBits - 1 downto accumulatorBits - 8)
      }
      is(op.Decay) {
        accumulator := accumulator + decayRom(io.d)
        decTmp := ((expOut * sustainVolume) >> 8).resized
        amplitude := decTmp.resized
      }
      is(op.Sustain) {
        amplitude := sustainVolume
        state := nextState
      }
      is(op.Release) {
        accumulator := accumulator + decayRom(io.r)
        relTmp := ((expOut * sustainVolume) >> 8).resized
        amplitude := relTmp.resized
          
        when (io.gate) {
          amplitude := 0
          accumulator := 0
          state := nextState
        }
      }
      default {
        amplitude := 0
        accumulator := 0
        state := nextState
      }
    }
  } 
}

