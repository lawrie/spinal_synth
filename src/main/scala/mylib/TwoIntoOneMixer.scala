package mylib

import spinal.core._
import spinal.lib._

class TwoIntoOneMixer(dataBits: Int = 12) extends Component {
  val io = new Bundle {
    val a = in SInt(dataBits bits)
    val b = in SInt(dataBits bits)
    val dout = out SInt(dataBits bits)
  }
  
  val intermediate = (io.a).resize(dataBits +1) + io.b.resize(dataBits+1)

  io.dout := (intermediate >> 1)
}

