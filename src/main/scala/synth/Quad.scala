package synth

import spinal.core._
import spinal.lib._

class Quad(width: Int) extends Component {
  val io = new Bundle {
    val quadA = in Bool
    val quadB = in Bool
    val initValue = in UInt(width bits)
    val position = out UInt(width bits)
  }

  val positionR = Reg(UInt(width bits)) init io.initValue
  io.position := positionR

  val quadAr = Reg(Bits(3 bits))
  val quadBr = Reg(Bits(3 bits))

  quadAr := quadAr(1 downto 0) ## io.quadA.asBits
  quadBr := quadBr(1 downto 0) ## io.quadB.asBits

  when (quadAr(2) ^ quadAr(1) ^ quadBr(2) ^ quadBr(1)) {
    when (quadBr(2) ^ quadAr(1)) {
      when (!positionR.andR) {
        positionR := positionR + 1
      }
    } otherwise {
      when (positionR.orR) {
        positionR := positionR - 1
      }
    }
  }
}

