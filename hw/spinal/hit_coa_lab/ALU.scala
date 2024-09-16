package hit_coa_lab

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import hit_coa_lab.OP.ADD

case class ALU() extends Component {
    val io = new Bundle {
        val flush = in Bool ()

        val data_sram_en = out Bool ()
        val data_sram_wen = out Bits (4 bits)
        val data_sram_addr = out UInt (32 bits)
        val data_sram_wdata = out Bits (32 bits)
        val data_sram_rdata = in Bits (32 bits)

        val decoded = slave Stream (new Bundle {
            val pc = UInt(32 bits)
            val op = OP()
            val src1 = Bits(32 bits)
            val src2 = Bits(32 bits)
            val src3 = Bits(32 bits)
            val src4 = Bits(32 bits)
            val wnum = UInt(5 bits)
            val wben = Bool()
        })

        val toWB = master Stream (new Bundle {
            val pc = UInt(32 bits)
            val wdata = Bits(32 bits)
            val wnum = UInt(5 bits)
            val wben = Bool()

            val target = UInt(32 bits)
            val jump = Bool()
            val jump_inst = Bool()
        })
        
        val toDEC = out(new Bundle {
            val running = Bool()
            val complete = Bool()
            val wnum = UInt(5 bits)
            val wben = Bool()
            val wdata = Bits(32 bits)
        })
    }

    val pc = Reg(UInt(32 bits)) init (0)
    val op = Reg(OP()) init (OP.DEFAULT)
    val src1 = Reg(Bits(32 bits)) init (0)
    val src2 = Reg(Bits(32 bits)) init (0)
    val src3 = Reg(Bits(32 bits)) init (0)
    val src4 = Reg(Bits(32 bits)) init (0)
    val wnum = Reg(UInt(5 bits)) init (0)
    val wben = Reg(Bool) init (False)
    val valid = Reg(Bool) init (False)

    when(io.flush) {
        valid := False
    }.elsewhen(io.decoded.valid && io.decoded.ready) {
        pc := io.decoded.payload.pc
        op := io.decoded.payload.op
        src1 := io.decoded.payload.src1
        src2 := io.decoded.payload.src2
        src3 := io.decoded.payload.src3
        src4 := io.decoded.payload.src4
        wnum := io.decoded.payload.wnum
        wben := io.decoded.payload.wben
        valid := True
    }.elsewhen(io.toWB.valid && io.toWB.ready) {
        valid := False
    }

    // ALU
    val ADD_result = src1.asUInt + src2.asUInt
    val SUB_result = src1.asUInt - src2.asUInt
    val AND_result = src1 & src2
    val OR_result = src1 | src2
    val XOR_result = src1 ^ src2
    val MOVZ_result = src1
    val SLL_result = src1 |<< src2(4 downto 0).asUInt
    val eq = src1 === src2
    val lts = src1.asSInt < src2.asSInt
    val ltu = src1.asUInt < src2.asUInt
    val les = src1.asSInt <= src2.asSInt
    val leu = src1.asUInt <= src2.asUInt
    val cmpr = (leu ## les ## ltu ## lts ## eq)
    val CMP_result = (B(0, 22 bits) ## ~cmpr ## cmpr)

    // BRU
    val target = U(0, 32 bits)
    val jump = False
    val jump_inst = False
    switch(op) {
        is(OP.J) {
            target := (src1(31 downto 28) ## src2(27 downto 0)).asUInt
            jump := True
            jump_inst := True
        }
        is(OP.BBT) {
            target := ADD_result
            jump := src3(src4(4 downto 0).asUInt)
            jump_inst := True
        }
    }

    // LSU
    val LSU_complete = True
    val lsu = new StateMachine {
        val idle = new State with EntryPoint
        val load = new State
        val store = new State

        io.data_sram_en := False
        io.data_sram_wen := B"0000"
        io.data_sram_addr := 0
        io.data_sram_wdata := 0

        idle
            .whenIsActive {
                when(valid && !io.flush) {
                    when(op === OP.LW) {
                        LSU_complete := False

                        io.data_sram_en := True
                        io.data_sram_wen := B"0000"
                        io.data_sram_addr := ADD_result
                        io.data_sram_wdata := 0

                        goto(load)
                    }.elsewhen(op === OP.SW) {
                        LSU_complete := False

                        io.data_sram_en := True
                        io.data_sram_wen := B"1111"
                        io.data_sram_addr := ADD_result
                        io.data_sram_wdata := src3

                        goto(store)
                    }
                }
            }

        load
            .whenIsActive {
                when(io.flush) {
                    goto(idle)
                }.elsewhen(io.toWB.ready) {
                    goto(idle)
                }.otherwise {
                    LSU_complete := False

                    io.data_sram_en := True
                    io.data_sram_wen := B"0000"
                    io.data_sram_addr := ADD_result
                    io.data_sram_wdata := 0
                }
            }

        store
            .whenIsActive {
                when(io.flush) {
                    goto(idle)
                }.elsewhen(io.toWB.ready) {
                    goto(idle)
                }.otherwise {
                    LSU_complete := False
                }
            }
    }

    val wdata = B(0, 32 bits)
    switch(op) {
        is(OP.ADD) {
            wdata := ADD_result.asBits
        }
        is(OP.SUB) {
            wdata := SUB_result.asBits
        }
        is(OP.AND) {
            wdata := AND_result
        }
        is(OP.OR) {
            wdata := OR_result
        }
        is(OP.XOR) {
            wdata := XOR_result
        }
        is(OP.MOVZ) {
            wdata := MOVZ_result
        }
        is(OP.SLL) {
            wdata := SLL_result
        }
        is(OP.CMP) {
            wdata := CMP_result
        }
        is(OP.LW) {
            wdata := io.data_sram_rdata
        }
    }

    io.decoded.ready := io.toWB.valid && io.toWB.ready || !valid
    io.toWB.valid := valid && LSU_complete && !io.flush
    io.toWB.payload.pc := pc
    io.toWB.payload.wdata := wdata
    io.toWB.payload.wnum := wnum
    io.toWB.payload.wben := wben
    io.toWB.payload.target := target
    io.toWB.payload.jump := jump
    io.toWB.payload.jump_inst := jump_inst
    
    io.toDEC.running := valid
    io.toDEC.complete := io.toWB.valid
    io.toDEC.wnum := wnum
    io.toDEC.wben := wben
    io.toDEC.wdata := wdata

}
