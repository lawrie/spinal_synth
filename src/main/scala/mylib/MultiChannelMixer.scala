package mylib

import spinal.core._
import spinal.lib._

class MultiChannelMixer(dataBits: Int = 12, activeChannels: Int = 2) extends Component {
  val io = new Bundle {
    val a = in SInt(dataBits bits)
    val b = in SInt(dataBits bits)
    val c = in SInt(dataBits bits)
    val d = in SInt(dataBits bits)
    val e = in SInt(dataBits bits)
    val f = in SInt(dataBits bits)
    val g = in SInt(dataBits bits)
    val h = in SInt(dataBits bits)
    val i = in SInt(dataBits bits)
    val j = in SInt(dataBits bits)
    val k = in SInt(dataBits bits)
    val l = in SInt(dataBits bits)
    val dout = out SInt(dataBits bits)
  }
  
  val extraBitsRequired = log2Up(activeChannels)
  val maxValue = (1 << dataBits - 1) - 1
  val minValue = - (1 << dataBits - 1)

  val sum = (io.a + io.b + io.c + io.d + io.e + io.f + io.g + 
    io.h + io.i + io.j + io.k + io.l) >> extraBitsRequired

  when (sum < minValue) {
    io.dout := minValue
  } elsewhen (sum > maxValue) {
    io.dout := maxValue
  } otherwise {
    io.dout := sum
  }
}

