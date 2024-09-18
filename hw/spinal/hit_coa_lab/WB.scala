package hit_coa_lab

import spinal.core._
import spinal.lib._

case class WB() extends Component {
    val io = new Bundle {
        val flush = out Bool ()
        val redirectPC = out UInt(32 bits)
        
        val fromALU = slave Stream (new Bundle {
            val pc = UInt(32 bits)
            val wdata = Bits(32 bits)
            val wnum = UInt(5 bits)
            val wben = Bool()
            
            val updateBPU = updateBPUBundle()
        })
        
        val updateBPU = master Stream(updateBPUBundle())
        
        val rf_wen = out Bool()
        val rf_wnum = out UInt(5 bits)
        val rf_wdata = out Bits(32 bits)
        
        val toDEC = out(new Bundle {
            val valid = Bool()
            val wnum = UInt(5 bits)
            val wben = Bool()
            val wdata = Bits(32 bits)
        })
        
        val debug = out(new Bundle {
            val pc = UInt(32 bits)
            val wdata = Bits(32 bits)
            val wnum = UInt(5 bits)
            val wen = Bool()
        })
    }
    
    val pc = Reg(UInt(32 bits)) init (0)    
    val wdata = Reg(Bits(32 bits)) init (0) 
    val wnum = Reg(UInt(5 bits)) init (0)
    val wben = Reg(Bool) init (False)
    val updateBPU = Reg(updateBPUBundle()) init (updateBPUBundle().rst())
    val valid = Reg(Bool) init (False)

    when (io.fromALU.valid && io.fromALU.ready) {
        pc := io.fromALU.payload.pc
        wdata := io.fromALU.payload.wdata
        wnum := io.fromALU.payload.wnum
        wben := io.fromALU.payload.wben
        updateBPU := io.fromALU.payload.updateBPU
        valid := True
    }.otherwise {
        valid := False
    }
    
    io.fromALU.ready := True && valid || !valid
    io.toDEC.valid := valid
    io.toDEC.wnum := wnum
    io.toDEC.wben := wben && valid
    io.toDEC.wdata := wdata
    
    io.rf_wen := wben && valid
    io.rf_wnum := wnum
    io.rf_wdata := wdata
    
    io.flush := valid && updateBPU.predict_fail
    io.redirectPC := updateBPU.jump ? updateBPU.target | (pc + 4)
    
    io.updateBPU.valid := valid
    io.updateBPU.payload := updateBPU
    
    io.debug.pc := pc
    io.debug.wdata := wdata
    io.debug.wnum := wnum
    io.debug.wen := wben && valid && io.debug.wnum =/= 0

}
