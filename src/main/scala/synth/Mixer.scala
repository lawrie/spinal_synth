package synth

import spinal.core._
import spinal.lib._

class Mixer(dataBits: Int = 12, activeChannels: Int = 2) extends Component {
  val io = new Bundle {
    val channel = in Vec(SInt(dataBits bits), activeChannels)
    val dout = out SInt(dataBits bits)
  }
  
  val extraBitsRequired = log2Up(activeChannels)
  val n = dataBits + extraBitsRequired

  val sum = Vec(SInt(n bits), activeChannels)

  sum(0)  := io.channel(0).resize(n)

  for(i <- 1 until activeChannels) {
    sum(i) := sum(i-1) + io.channel(i).resize(n)
  }

  io.dout := sum(activeChannels - 1) >> extraBitsRequired
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
