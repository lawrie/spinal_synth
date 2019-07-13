package synth

import spinal.core._
import spinal.lib._

class AmplitudeModulator(dataBits: Int = 12, amplitudeBits: Int = 8) extends Component {
  val io = new Bundle {
    val sampleClk = in Bool
    val din = in SInt(dataBits bits)
    val amplitude = in UInt(amplitudeBits bits)
    val dout = out SInt(dataBits bits)
  }

  val sampleDomain = new ClockDomain(
    clock=io.sampleClk,
    config=ClockDomainConfig(resetKind=BOOT)
  )

  val sampleArea = new ClockingArea(sampleDomain) {

    val scaledDin = Reg(SInt(dataBits + amplitudeBits bits))
  
    scaledDin := (io.din * (S"0" @@ io.amplitude.asSInt)).resized

    io.dout := scaledDin(amplitudeBits + dataBits - 1 downto amplitudeBits) addTag(crossClockDomain)
  }
}

