package hit_coa_lab

import spinal.core._
import spinal.lib._

// Hardware definition
case class MyTopLevel() extends Component {
    val io = new Bundle {
        val clk = in Bool ()
        val resetn = in Bool ()

        val inst_sram_en = out Bool ()
        val inst_sram_addr = out UInt (32 bits)
        val inst_sram_rdata = in Bits (32 bits)

        val data_sram_en = out Bool ()
        val data_sram_wen = out Bits (4 bits)
        val data_sram_addr = out UInt (32 bits)
        val data_sram_wdata = out Bits (32 bits)
        val data_sram_rdata = in Bits (32 bits)

        val debug_wb_pc = out UInt (32 bits)
        val debug_wb_rf_wen = out Bool ()
        val debug_wb_rf_wnum = out UInt (5 bits)
        val debug_wb_rf_wdata = out Bits (32 bits)
        // val debug_wb_rf_inst = out Bits (32 bits)
    }
    noIoPrefix()
    setDefinitionName("cpu")
    
    // clock domain
    val default_clock_domain = ClockDomain(
        clock = io.clk,
        reset = io.resetn,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = LOW
        )
    )
    
    // area
    val default_area = new ClockingArea(default_clock_domain) {
        val pc = new PC()
        val fetch = new FETCH()
        val decode = new DECODE()
        val alu = new ALU()
        val wb = new WB()
        val rf = new REGFILE()
        
        // connect
        pc.io.flush <> wb.io.flush
        pc.io.redirectPC <> wb.io.redirectPC
        pc.io.toFetch <> fetch.io.fromPC
        
        fetch.io.flush <> wb.io.flush
        fetch.io.toDecode <> decode.io.fromFetch
        fetch.io.inst_sram_en <> io.inst_sram_en
        fetch.io.inst_sram_addr <> io.inst_sram_addr
        fetch.io.inst_sram_rdata <> io.inst_sram_rdata

        decode.io.flush <> wb.io.flush
        decode.io.decoded <> alu.io.decoded
        decode.io.rf_raddr1 <> rf.io.rf_raddr1
        decode.io.rf_raddr2 <> rf.io.rf_raddr2
        decode.io.rf_rdata1 <> rf.io.rf_rdata1
        decode.io.rf_rdata2 <> rf.io.rf_rdata2
        
        alu.io.flush <> wb.io.flush
        alu.io.data_sram_en <> io.data_sram_en
        alu.io.data_sram_wen <> io.data_sram_wen
        alu.io.data_sram_addr <> io.data_sram_addr
        alu.io.data_sram_wdata <> io.data_sram_wdata
        alu.io.data_sram_rdata <> io.data_sram_rdata
        alu.io.toWB <> wb.io.fromALU
        alu.io.toDEC <> decode.io.fromALU
        
        wb.io.rf_wen <> rf.io.rf_wen
        wb.io.rf_wnum <> rf.io.rf_wnum
        wb.io.rf_wdata <> rf.io.rf_wdata
        wb.io.toDEC <> decode.io.fromWB
        wb.io.updateBPU <> pc.io.updateBPU

        io.debug_wb_pc <> wb.io.debug.pc
        io.debug_wb_rf_wen <> wb.io.debug.wen
        io.debug_wb_rf_wnum <> wb.io.debug.wnum
        io.debug_wb_rf_wdata <> wb.io.debug.wdata
        
    }

}

object MyTopLevelVerilog extends App {
    Config.spinal.generateVerilog(MyTopLevel())
    // Config.spinal.generateVerilog(PC())
}

object MyTopLevelVhdl extends App {
    Config.spinal.generateVhdl(MyTopLevel())
}
