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
  val n = dataBits + extraBitsRequired

  val sum = (io.a.resize(n) + io.b.resize(n) + io.c.resize(n) + io.d.resize(n) + 
              io.e.resize(n) + io.f.resize(n) + io.g.resize(n) + io.h.resize(n) + 
              io.i.resize(n) + io.j.resize(n) + io.k.resize(n) + io.l.resize(n)) >> extraBitsRequired

  io.dout := sum
}

