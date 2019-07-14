package synth

import spinal.core._
import spinal.lib._

class FilterEwma(dataBits: Int = 12) extends Component {
  val io = new Bundle {
    val sampleClk = in Bool
    val sAlpha = in SInt(9 bits)
    val din = in SInt(dataBits bits)
    val dout = out SInt(dataBits bits)
  }

  val sampleDomain = new ClockDomain(
    clock=io.sampleClk,
    config=ClockDomainConfig(resetKind=BOOT)
  )

  val sampleArea = new ClockingArea(sampleDomain) {

    val dout = Reg(SInt(dataBits bits))
    io.dout := dout addTag(crossClockDomain)

    val sAdder1Out = Reg(SInt(dataBits + 1 bits))
    sAdder1Out := io.din.resize(dataBits + 1) - dout.resize(dataBits + 1)

    val swRawMulOutput = (sAdder1Out * io.sAlpha) >> 8
    val sMulOut = swRawMulOutput(dataBits downto 0)
    val tmpDout = sMulOut + dout

    dout := tmpDout(dataBits-1 downto 0)
  }
}
