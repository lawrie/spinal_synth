package mylib

import spinal.core._
import spinal.lib._

class FilterEwma(dataBits: Int = 12) extends Component {
  val io = new Bundle {
    val sAlpha = in SInt(9 bits)
    val din = in SInt(dataBits bits)
    val dout = out SInt(dataBits bits)
  }

  val dout = Reg(SInt(dataBits bits))
  io.dout := dout

  val sAdder1Out = io.din - dout
  val swRawMulOutput = (sAdder1Out * io.sAlpha) >> 8
  val sMulOut = swRawMulOutput(dataBits downto 0)
  val tmpDout = sMulOut + dout
  dout := tmpDout(dataBits-1 downto 0)
}
