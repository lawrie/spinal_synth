package mylib

import spinal.core._
import spinal.lib._

class AmplitudeModulator(dataBits: Int = 12, amplitudeBits: Int = 8) extends Component {
  val io = new Bundle {
    val din = in SInt(dataBits bits)
    val amplitude = in UInt(amplitudeBits bits)
    val dout = out SInt(dataBits bits)
  }

  val ampSigned = (B"0" ## io.amplitude).asSInt

  val scaledDin = Reg(SInt(dataBits + amplitudeBits bits))

  scaledDin := (io.din * ampSigned).resized

  io.dout := scaledDin(amplitudeBits + dataBits - 1 downto amplitudeBits)
}

