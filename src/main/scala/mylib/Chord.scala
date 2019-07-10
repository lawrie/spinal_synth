package mylib

import spinal.core._
import spinal.lib._

class Chord(dataBits: Int = 12) extends Component {
  val io = new Bundle {
    val gate = in Bool
    val audio = out Bool
  }

  val clockHz = 100000000
  val oneMHzClk = new ClkDivider(clockHz / 1000000)
  val sampleClk = new ClkDivider(clockHz / 44100)

  val oneMHzDomain = new ClockDomain(clock=oneMHzClk.io.cout, reset=clockDomain.reset)
  val pdm = new Pdm(dataBits)

  val oneMHzArea = new ClockingArea(oneMHzDomain) {
    val voiceC = new Voice(outputBits = dataBits)
    voiceC.io.sampleClk := sampleClk.io.cout
    voiceC.io.toneFreq := 4389
    voiceC.io.pulseWidth :=  2047
    voiceC.io.waveFormEnable := B"0001"
    voiceC.io.attack := U"0010"
    voiceC.io.decay := U"0010"
    voiceC.io.sustain := U"1000"
    voiceC.io.release := U"1100"
    voiceC.io.gate := !io.gate

    val voiceF = new Voice(outputBits = dataBits)
    voiceF.io.sampleClk := sampleClk.io.cout
    voiceF.io.toneFreq := 5530
    voiceF.io.pulseWidth :=  2047
    voiceF.io.waveFormEnable := B"0001"
    voiceF.io.attack := U"0010"
    voiceF.io.decay := U"0010"
    voiceF.io.sustain := U"1000"
    voiceF.io.release := U"1100"
    voiceF.io.gate := !io.gate

    val voiceG = new Voice(outputBits = dataBits)
    voiceG.io.sampleClk := sampleClk.io.cout
    voiceG.io.toneFreq := 6577
    voiceG.io.pulseWidth :=  2047
    voiceG.io.waveFormEnable := B"0001"
    voiceG.io.attack := U"0010"
    voiceG.io.decay := U"0010"
    voiceG.io.sustain := U"1000"
    voiceG.io.release := U"1100"
    voiceG.io.gate := !io.gate

    val intermediateMixer = new TwoIntoOneMixer(dataBits = dataBits)
    intermediateMixer.io.a := voiceC.io.dout
    intermediateMixer.io.b := voiceF.io.dout

    val finalMixer = new TwoIntoOneMixer(dataBits = dataBits)
    finalMixer.io.a := intermediateMixer.io.dout
    finalMixer.io.b := voiceG.io.dout

    pdm.io.din := finalMixer.io.dout addTag(crossClockDomain)
  }

  io.audio := pdm.io.dout
}

object ChordTest {
  def main(args: Array[String]) {
    SpinalVerilog(new Chord(12))
  }
}
