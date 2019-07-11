package mylib

import spinal.core._
import spinal.lib._

class Chord(freq1: Int = 4389, freq2: Int = 5530, freq3: Int = 6577, 
            dataBits: Int = 12, clockHz: Int = 100000000) extends Component {
  val io = new Bundle {
    val clk = in Bool
    val gate = in Bool
    val audio = out Bool
  }

  val pdmClockDomain = new ClockDomain(
    clock=io.clk, 
    config = ClockDomainConfig(resetKind=BOOT)
  )

  val pdmArea = new ClockingArea(pdmClockDomain) {
    val oneMHzClk = new ClkDivider(clockHz / 1000000)
    val sampleClk = new ClkDivider(clockHz / 44100)

    val oneMHzDomain = new ClockDomain(
      clock=oneMHzClk.io.cout,
      config = ClockDomainConfig(resetKind=BOOT)
    )

    val pdm = new Pdm(dataBits)

    val oneMHzArea = new ClockingArea(oneMHzDomain) {
      def voice(freq: Int): Voice = {
        val voice = new Voice(outputBits = dataBits)
        voice.io.sampleClk := sampleClk.io.cout
        voice.io.toneFreq := freq
        voice.io.pulseWidth :=  2047
        voice.io.waveFormEnable := B"0001"
        voice.io.attack := U"0010"
        voice.io.decay := U"0010"
        voice.io.sustain := U"1000"
        voice.io.release := U"1100"
        voice.io.gate := !io.gate

        voice
      }

      val voice1 = voice(freq1)
      val voice2 = voice(freq2)
      val voice3 = voice(freq3)

      val mixer = new Mixer(dataBits = dataBits, activeChannels = 3)
      mixer.io.channel(0) := voice1.io.dout
      mixer.io.channel(1) := voice2.io.dout
      mixer.io.channel(2) := voice3.io.dout

      pdm.io.din := mixer.io.dout addTag(crossClockDomain)
    }

    io.audio := pdm.io.dout
  }
}

object ChordTest {
  def main(args: Array[String]) {
    SpinalVerilog(new Chord(dataBits = 12))
  }
}
