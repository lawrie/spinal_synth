package mylib

import spinal.core._
import spinal.lib._

class FilterSvf(sampleBits: Int = 12) extends Component {
  val io = new Bundle {
    val sin = in SInt(sampleBits bits)
    val f = in SInt(18  bits)
    val q1 = in SInt(18  bits)
    val highPass = out SInt(sampleBits bits)
    val lowPass = out SInt(sampleBits bits)
    val bandPass = out SInt(sampleBits bits)
    val notch = out SInt(sampleBits bits)
  }

  val highPass = Reg(SInt(sampleBits+3 bits))
  val lowPass = Reg(SInt(sampleBits+3 bits))
  val bandPass = Reg(SInt(sampleBits+3 bits))
  val notch = Reg(SInt(sampleBits+3 bits))

  io.highPass := clamp(highPass)
  io.lowPass := clamp(lowPass)
  io.bandPass := clamp(bandPass)
  io.notch := clamp(notch)

  val minValue = S(- (1 << (sampleBits -1))) 
  val maxValue = S(1 << (sampleBits -1) - 1)

  def clamp(x: SInt): SInt = {
    val t = (x < minValue) ? minValue | x
    (t > maxValue) ? maxValue | t
  }

  val inSignedExtended = (io.sin(sampleBits-1) ## io.sin(sampleBits-1) ## io.sin(sampleBits-1) ## io.sin).asSInt
  val q1ScaledDelayedBandPass = (bandPass * io.q1) >> 16
  val fScaledDelayedBandPass = (bandPass * io.f) >> 17
  lowPass := lowPass + fScaledDelayedBandPass(sampleBits + 2 downto 0)
  highPass := inSignedExtended - lowPass - q1ScaledDelayedBandPass(sampleBits + 2 downto 0)
  
  val fScaledHighPass = (highPass * io.f) >> 17
  bandPass := fScaledHighPass(sampleBits+2 downto 0) + bandPass
  notch := highPass + lowPass
}
