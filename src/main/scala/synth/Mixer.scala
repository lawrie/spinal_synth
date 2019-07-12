package synth

import spinal.core._
import spinal.lib._

class Mixer(dataBits: Int = 12, numChannels: Int = 2, activeChannels: Int = 2) extends Component {
  val io = new Bundle {
    val channel = in Vec(SInt(dataBits bits), numChannels)
    val dout = out SInt(dataBits bits)
  }

  val maxValue = S((1 << dataBits) - 1)
  val minValue = S(- (1 << (dataBits - 1)))

  val extraBitsRequired = log2Up(numChannels)
  val n = dataBits + extraBitsRequired

  // Active channels is average number active and can be less than the
  // number of channels. So shift can be less, than extraBitsRequired
  // to avoid too much reduction in volume.
  val shift = log2Up(activeChannels)

  val sum = Vec(SInt(n bits), numChannels)

  sum(0)  := io.channel(0).resize(n)

  for(i <- 1 until numChannels) {
    sum(i) := sum(i-1) + io.channel(i).resize(n)
  }

  // Sum might need truncating as could be bigger than the available bits due to short shift
  val sumOut = sum(numChannels - 1) >> shift
  val temp = (sumOut > maxValue) ? maxValue | sumOut
  io.dout := ((temp < minValue) ? minValue | sumOut).resized
}

class MixerTest(dataBits: Int = 12, activeChannels: Int = 2) extends Component {
  val io = new Bundle {
    val dout = out SInt(dataBits bits)
  }

  val mixer = new Mixer(dataBits = dataBits, activeChannels = activeChannels)
  mixer.io.channel(0) := 0x23
  mixer.io.channel(1) := 0x42
  mixer.io.channel(2) := 0
  mixer.io.channel(3) := 0xff
  io.dout := mixer.io.dout
}

object MixerTest {
  def main(args: Array[String]) {
    SpinalVerilog(new MixerTest(activeChannels=4))
  }
}
