package hit_coa_lab

import spinal.core._
import spinal.lib._

case class PC() extends Component {
    val io = new Bundle {
        val flush = in Bool()
        val redirectPC = in UInt(32 bits)

        val toFetch = master Stream(new Bundle {
            val pc = UInt(32 bits)
        })
    }
    
    val pc = Reg(UInt(32 bits)) init(0)
    val pc_valid = Reg(Bool) init(True)
    val pc_next = pc + 4

    when (io.flush) {
        pc := io.redirectPC
        pc_valid := True
    } .elsewhen(io.toFetch.ready && io.toFetch.valid) {
        pc := pc_next
        pc_valid := True
    }
    
    io.toFetch.valid := pc_valid && !io.flush
    io.toFetch.payload.pc := pc
}
