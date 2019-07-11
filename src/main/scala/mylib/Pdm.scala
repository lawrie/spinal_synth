package mylib

import spinal.core._
import spinal.lib._

class Pdm(dataBits: Int = 12) extends Component {
  val io = new Bundle {
    val din = in SInt(dataBits bits)
    val dout = out Bool
    val accOut = out UInt(dataBits + 1 bits)
  }

  val accumulator = Reg(UInt(dataBits+1 bits))
  io.accOut := accumulator
  
  val unsignedDin = (io.din.asUInt ^ (1 << (dataBits-1)))

  accumulator := accumulator(dataBits-1 downto 0).resize(dataBits+1) + unsignedDin.resize(dataBits+1)
  io.dout := accumulator(dataBits)
}

class ClkDivider(divisor: Int) extends Component {
  val io = new Bundle {
    val cout = out Bool
  }

  val counter = Reg(UInt(28 bits)) init 0
  val increment = (1 << 28) / divisor

  counter := counter + increment
  io.cout := counter(27)
}

class PdmTest(dataBits: Int = 12) extends Component {
  val io = new Bundle {
    val clk = in Bool
    val audio = out Bool
    val leds = out Bits(8 bits)
  }

  val pdmClockDomain = new ClockDomain(
    clock=io.clk,
    config = ClockDomainConfig(resetKind=BOOT)
  )

  val pdmArea = new ClockingArea(pdmClockDomain) {
    val clockHz = 100000000
    val bpm = 120
    val tickHz = ((bpm * 4) / 60) * 8
    //val tickHz = 1000000

    val oneMHzClk = new ClkDivider(clockHz / 1000000)
    val sampleClk = new ClkDivider(clockHz / 44100)
    val tickClk = new ClkDivider(clockHz / tickHz)

    val oneMHzDomain = new ClockDomain(
      clock=oneMHzClk.io.cout, 
      config=ClockDomainConfig(resetKind=BOOT)
    )
    val pdm = new Pdm(dataBits)

    val oneMHzArea = new ClockingArea(oneMHzDomain) {
      val songPlayer = new SongPlayer(dataBits = 12)
      songPlayer.io.sampleClk := sampleClk.io.cout
      songPlayer.io.tickClk := tickClk.io.cout

      pdm.io.din := songPlayer.io.dout addTag(crossClockDomain)
      io.leds := songPlayer.io.diag
    }

    //io.leds := pdm.io.accOut(12 downto 5).asBits
    io.audio := pdm.io.dout
  }
}

object PdmTest {
  def main(args: Array[String]) {
    SpinalVerilog(new PdmTest(12))
  }
}

object PdmSim {
  import spinal.core.sim._

  def main(args: Array[String]) {
    SimConfig.withWave.compile(new PdmTest(12)).doSim{ dut =>
      dut.clockDomain.forkStimulus(100)

      dut.clockDomain.waitSampling(100000)
    }
  }
}


