package hit_coa_lab

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import scala.tools.nsc.doc.html.HtmlTags.Tr

case class FETCH() extends Component {
    val io = new Bundle {
        val flush = in Bool ()

        val inst_sram_en = out Bool
        val inst_sram_addr = out UInt (32 bits)
        val inst_sram_rdata = in Bits (32 bits) // 1 cycle delay

        val fromPC = slave Stream (new Bundle {
            val pc = UInt(32 bits)
        })

        val toDecode = master Stream (new Bundle {
            val pc = UInt(32 bits)
            val inst = Bits(32 bits)
        })
    }

    val pc = io.fromPC.pc
    val pc_reg = Reg(UInt(32 bits)) init (0)

    val reader = new StateMachine {
        val request = new State with EntryPoint
        val accept = new State
        
        io.inst_sram_en := False
        io.inst_sram_addr := 0
        
        io.fromPC.ready := False
        io.toDecode.valid := False
        io.toDecode.payload.pc := 0
        io.toDecode.payload.inst := 0

        request
            .whenIsActive {
                io.fromPC.ready := True || io.flush
                io.toDecode.valid := False

                when(io.fromPC.valid && io.fromPC.ready) {
                    io.inst_sram_en := True
                    io.inst_sram_addr := pc
                    pc_reg := pc
                    goto(accept)
                }
            }

        accept
            .whenIsActive {
                io.fromPC.ready := False
                io.toDecode.valid := True && !io.flush

                when(io.flush) {
                    goto(request)
                }.elsewhen(io.toDecode.ready) {
                    io.inst_sram_en := False
                    io.toDecode.payload.pc := pc_reg
                    io.toDecode.payload.inst := io.inst_sram_rdata
                    goto(request)
                }.otherwise {
                    io.inst_sram_en := True
                    io.inst_sram_addr := pc_reg
                }
            }

    }

}
