package hit_coa_lab

import spinal.core._
import spinal.lib._

case class REGFILE() extends Component {
    val io = new Bundle {
        val rf_wen = in Bool()
        val rf_wnum = in UInt(5 bits)
        val rf_wdata = in Bits(32 bits)
        val rf_raddr1 = in UInt(5 bits)
        val rf_raddr2 = in UInt(5 bits)
        val rf_rdata1 = out Bits(32 bits)
        val rf_rdata2 = out Bits(32 bits)
    }

    // val rf = Mem(Bits(32 bits), 32)
    val rf = Vec.fill(32)(Reg(Bits(32 bits)) init(0))
    when(io.rf_wen) {
        rf(io.rf_wnum) := io.rf_wdata
    }

    // io.rf_rdata1 := (io.rf_raddr1 === 0) ? B(0) | rf.readAsync(io.rf_raddr1)
    // io.rf_rdata2 := (io.rf_raddr2 === 0) ? B(0) | rf.readAsync(io.rf_raddr2)
    io.rf_rdata1 := (io.rf_raddr1 === 0) ? B(0) | rf(io.rf_raddr1)
    io.rf_rdata2 := (io.rf_raddr2 === 0) ? B(0) | rf(io.rf_raddr2)
}
