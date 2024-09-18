package hit_coa_lab

import spinal.core._
import spinal.lib._

case class DECODE() extends Component {
    val io = new Bundle {
        val flush = in Bool ()

        val fromFetch = slave Stream (new Bundle {
            val pc = UInt(32 bits)
            val inst = Bits(32 bits)
            val bpuBundle = BPUBundle()
        })

        val decoded = master Stream (new Bundle {
            val pc = UInt(32 bits)
            val op = OP()
            val src1 = Bits(32 bits)
            val src2 = Bits(32 bits)
            val src3 = Bits(32 bits)
            val src4 = Bits(32 bits)
            val wnum = UInt(5 bits)
            val wben = Bool()
            val bpuBundle = BPUBundle()
        })

        val fromALU = in(new Bundle {
            val running = Bool()
            val complete = Bool()
            val wnum = UInt(5 bits)
            val wben = Bool()
            val wdata = Bits(32 bits)
        })

        val fromWB = in(new Bundle {
            val valid = Bool()
            val wnum = UInt(5 bits)
            val wben = Bool()
            val wdata = Bits(32 bits)
        })

        val rf_raddr1 = out UInt (5 bits)
        val rf_raddr2 = out UInt (5 bits)
        val rf_rdata1 = in Bits (32 bits)
        val rf_rdata2 = in Bits (32 bits)
    }

    val pc = Reg(UInt(32 bits)) init (0)
    val inst = Reg(Bits(32 bits)) init (0)
    val valid = Reg(Bool) init (False)
    val bpuBundle = Reg(BPUBundle()) init (BPUBundle().rst())
    when(io.flush) {
        valid := False
    }.elsewhen(io.fromFetch.valid && io.fromFetch.ready) {
        pc := io.fromFetch.payload.pc
        inst := io.fromFetch.payload.inst
        bpuBundle := io.fromFetch.payload.bpuBundle
        valid := True
    }.elsewhen(io.decoded.valid && io.decoded.ready) {
        valid := False
    }

    val rs = inst(25 downto 21)
    val rt = inst(20 downto 16)
    val rd = inst(15 downto 11)
    val offset = (inst(15) #* 16 ## inst(15 downto 0))
    val instr_index = inst(25 downto 0).resize(32)
    val sa = inst(10 downto 6).resize(32)
    val BBTimm = inst(20 downto 16).resize(32)

    io.rf_raddr1 := rs.asUInt
    io.rf_raddr2 := rt.asUInt

    val ALU_hit_rs = io.fromALU.running && io.fromALU.wben && io.fromALU.wnum === rs.asUInt && io.fromALU.wnum =/= 0
    val ALU_hit_rt = io.fromALU.running && io.fromALU.wben && io.fromALU.wnum === rt.asUInt && io.fromALU.wnum =/= 0
    val WB_hit_rs = io.fromWB.valid && io.fromWB.wben && io.fromWB.wnum === rs.asUInt && io.fromWB.wnum =/= 0
    val WB_hit_rt = io.fromWB.valid && io.fromWB.wben && io.fromWB.wnum === rt.asUInt && io.fromWB.wnum =/= 0

    val rs_rdata = Mux(ALU_hit_rs, io.fromALU.wdata, Mux(WB_hit_rs, io.fromWB.wdata, io.rf_rdata1))
    val rt_rdata = Mux(ALU_hit_rt, io.fromALU.wdata, Mux(WB_hit_rt, io.fromWB.wdata, io.rf_rdata2))

    val op = OP()
    val src1 = B(0, 32 bits)
    val src2 = B(0, 32 bits)
    val src3 = B(0, 32 bits)
    val src4 = B(0, 32 bits)
    val wnum = U(0, 5 bits)
    val wben = False

    val stall = !io.fromALU.complete && (ALU_hit_rs || ALU_hit_rt)

    switch(inst) {
        is(INST.ADD) {
            op := OP.ADD
            src1 := rs_rdata
            src2 := rt_rdata
            wnum := rd.asUInt
            wben := True
        }
        is(INST.SUB) {
            op := OP.SUB
            src1 := rs_rdata
            src2 := rt_rdata
            wnum := rd.asUInt
            wben := True
        }
        is(INST.AND) {
            op := OP.AND
            src1 := rs_rdata
            src2 := rt_rdata
            wnum := rd.asUInt
            wben := True
        }
        is(INST.OR) {
            op := OP.OR
            src1 := rs_rdata
            src2 := rt_rdata
            wnum := rd.asUInt
            wben := True
        }
        is(INST.XOR) {
            op := OP.XOR
            src1 := rs_rdata
            src2 := rt_rdata
            wnum := rd.asUInt
            wben := True
        }
        is(INST.SW) {
            op := OP.SW
            src1 := rs_rdata
            src2 := offset
            src3 := rt_rdata
        }
        is(INST.LW) {
            op := OP.LW
            src1 := rs_rdata
            src2 := offset
            wnum := rt.asUInt
            wben := True
        }
        is(INST.J) {
            op := OP.J
            src1 := (pc + 4).asBits
            src2 := instr_index
        }
        is(INST.MOVZ) {
            op := OP.MOVZ
            src1 := rs_rdata
            src2 := rt_rdata
            wnum := rd.asUInt
            wben := rt_rdata === 0
        }
        is(INST.SLL) {
            op := OP.SLL
            src1 := rt_rdata
            src2 := sa
            wnum := rd.asUInt
            wben := True
        }
        is(INST.CMP) {
            op := OP.CMP
            src1 := rs_rdata
            src2 := rt_rdata
            wnum := rd.asUInt
            wben := True
        }
        is(INST.BBT) {
            op := OP.BBT
            src1 := (pc + 4).asBits
            src2 := offset |<< 2
            src3 := rs_rdata
            src4 := BBTimm
        }
        default {
            op := OP.DEFAULT
        }
    }

    io.fromFetch.ready := io.decoded.valid && io.decoded.ready || !valid
    io.decoded.valid := valid && !stall && !io.flush

    io.decoded.payload.pc := pc
    io.decoded.payload.op := op
    io.decoded.payload.src1 := src1
    io.decoded.payload.src2 := src2
    io.decoded.payload.src3 := src3
    io.decoded.payload.src4 := src4
    io.decoded.payload.wnum := wnum
    io.decoded.payload.wben := wben
    
    io.decoded.payload.bpuBundle := bpuBundle

}
