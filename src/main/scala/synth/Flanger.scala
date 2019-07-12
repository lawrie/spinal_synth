package synth

import spinal.core._
import spinal.lib._

case class SB_RAM40_4K() extends BlackBox {
  addGeneric("WRITE_MODE", "0")
  addGeneric("READ_MODE", "0")

  val RDATA = out SInt(16 bits)
  val RADDR = in UInt(8 bits)
  val RCLK = in Bool
  val RE = in Bool
  val WADDR = in UInt(8 bits)
  val WDATA = in SInt(16 bits)
  val WCLK = in Bool
  val WE = in Bool
}

class Flanger(delayBufferLengthBits: Int = 8, sampleBits: Int = 12, sampleRate: Int = 44100,
              flangRate: Double = 1.4, accumulatorBits: Int = 21) extends Component {
  val io = new Bundle {
    val sampleClk = in Bool
    val din = in SInt(sampleBits bits)
    val dout = out SInt(sampleBits bits)
  }

  val delayBufferLength = (1 << delayBufferLengthBits)
  val delayBufferMax = delayBufferLength - 1

  val sampleBitHigh = (1 << (sampleBits - 1))

  val accumulator = Reg(UInt(accumulatorBits bits))
  val delayBufferWriteAddress = Reg(UInt(8 bits))

  val delayTapOutput = Reg(SInt(sampleBits+1 bit))

  val delayBufferTemp = accumulator(accumulatorBits -2 downto accumulatorBits - 1 - delayBufferLengthBits)
  val delayBufferTapIndex = accumulator.msb ? ~delayBufferTemp | delayBufferTemp

  val delayBufferReadAddress = (delayBufferWriteAddress - delayBufferTapIndex) & delayBufferMax

  val accumulatorMaxScale = (1 << accumulatorBits).toDouble
  val accumulatorPhaseIncrement = ((accumulatorMaxScale * flangRate) / 44100).toInt
  
  val delayBuffer = SB_RAM40_4K()
  delayBuffer.RADDR := delayBufferReadAddress
  delayBuffer.WADDR := delayBufferWriteAddress
  delayBuffer.RCLK := io.sampleClk
  delayBuffer.WCLK := io.sampleClk
  delayBuffer.RE := True
  delayBuffer. WE := True
  delayBuffer.WDATA := io.din.resized
  delayTapOutput := delayBuffer.RDATA.resized

  val sampleDomain = new ClockDomain(clock=io.sampleClk) 

  val sampleArea = new ClockingArea(sampleDomain) {
    delayBufferWriteAddress := delayBufferWriteAddress + 1
    accumulator := accumulator + accumulatorPhaseIncrement
    
    io.dout := (io.din.resize(sampleBits+1) + delayTapOutput) >> 1
  }
}
