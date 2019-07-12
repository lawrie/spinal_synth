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

abstract class PlayerComponent extends Component {
    val io = new Bundle {
    val sampleClk = in Bool
    val tickClk = in Bool
    val dout = out SInt(12 bits)
    val diag = out Bits(8 bits)
    val gate = in Bool
    val switches = in Bits(4 bits)
    val quadA = in Bool
    val quadB = in Bool
  }
}

class PdmPlayer[T: Manifest](dataBits: Int = 12) extends Component {
  val io = new Bundle {
    val clk = in Bool
    val audio = out Bool
    val leds = out Bits(8 bits)
    val gate = in Bool
    val switches = in Bits(4 bits)
    val quadA = in Bool
    val quadB = in Bool
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
      val player = manifest[T].erasure.newInstance().asInstanceOf[PlayerComponent]
      player.io.sampleClk := sampleClk.io.cout
      player.io.tickClk := tickClk.io.cout
      player.io.gate := io.gate
      player.io.switches := io.switches
      player.io.quadA := io.quadA
      player.io.quadB := io.quadB

      pdm.io.din := player.io.dout addTag(crossClockDomain)
      io.leds := player.io.diag
    }

    io.audio := pdm.io.dout
  }
}

object PdmPlayer {
  def main(args: Array[String]) {
    SpinalVerilog(new PdmPlayer[SongPlayer](dataBits = 12))
  }
}

object PdmPlayerSim {
  import spinal.core.sim._

  def main(args: Array[String]) {
    SimConfig.withWave.compile(new PdmPlayer[SongPlayer](dataBits = 12)).doSim{ dut =>
      dut.clockDomain.forkStimulus(100)

      dut.clockDomain.waitSampling(100000)
    }
  }
}


