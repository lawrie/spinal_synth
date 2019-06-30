package mylib

import spinal.core._
import spinal.lib._

class SongPlayer(dataBits: Int = 12) extends Component {
  val io = new Bundle {
    val sampleClk = in Bool
    val tickClk = in Bool
    val dout = out SInt(dataBits bits)
  }

  val tune = Array(U(1), U(1), U(8), U(8), U(10), U(10), U(8), U(0))
  val notes = Array(U(0), U(17557), U(18601), U(19709), U(20897), U(22121), U(23436), U(24830),
                   U(26306), U(27871),U(29528), U(31234), U(33144))
  val tuneRom = Mem(UInt(4 bits), tune)
  val notesRom = Mem(UInt(16 bits), notes)

  val songPosition = Reg(UInt(8 bits)) init 0
  val barPosition = Reg(UInt(8 bits)) init 0

  val channel1 = new Voice(outputBits = dataBits)
  channel1.io.sampleClk := io.sampleClk
  io.dout := channel1.io.dout
  channel1.io.pulseWidth := 2048
  channel1.io.waveFormEnable := B"0110"
  channel1.io.attack := U"0011"
  channel1.io.decay := U"0000"
  channel1.io.sustain := U"1111"
  channel1.io.release := U"0110"

  val tickDomain = new ClockDomain(clock=io.tickClk) 

  val tickArea = new ClockingArea(tickDomain) {
    val toneFreq = Reg(UInt(16 bits))
    val tickTimer = Reg(UInt(2 bits))
    val noteCounter = Reg(UInt(3 bits)) 
    val gate = Reg(Bool)
  
    channel1.io.toneFreq := toneFreq addTag(crossClockDomain)
    channel1.io.gate := gate addTag(crossClockDomain)

    tickTimer := tickTimer + 1

    when (tickTimer === 0) {
      gate := True
      toneFreq := notesRom(tuneRom(noteCounter))
      noteCounter := noteCounter + 1
    } elsewhen (tickTimer === 3) {
      gate := False
    }
  }
}

