package hit_coa_lab

import spinal.core._
import spinal.lib._

case class PC() extends Component {
    val io = new Bundle {
        val flush = in Bool()
        val redirectPC = in UInt(32 bits)

        val toFetch = master Stream(new Bundle {
            val pc = UInt(32 bits)
            val bpuBundle = BPUBundle()
        })

        val updateBPU = slave Stream(updateBPUBundle())
    }
    
    val pc = Reg(UInt(32 bits)) init(0)
    val pc_valid = Reg(Bool) init(True)
    val pc_plus_4 = pc + 4
    
    val ghr = Reg(UInt(10 bits)) init(0)
    val pbtb = Mem(btbBundle(), 1024)
    val ubtb = Mem(btbBundle(), 1024)
    val btb_valid_list = Reg(Bits(1024 bits)) init(0)
    val pbht = Mem(UInt(2 bits), 1024)
    val ubht = Mem(UInt(2 bits), 1024)
    val bht_valid_list = Reg(Bits(1024 bits)) init(0)
    
    val btb_index = pc(11 downto 2)
    val bht_index = pc(11 downto 2) ^ ghr
    val btb_item = pbtb.readAsync(btb_index)
    val bht_item = pbht.readAsync(bht_index)
    val btb_valid = btb_valid_list(btb_index)
    val bht_valid = bht_valid_list(bht_index)
    val btb_hit = btb_valid ? (btb_item.tag === pc(21 downto 12)) | False
    val bht_hit = bht_valid
    val predict_target = btb_hit ? btb_item.target | (pc+4)
    val predict_jump = (bht_hit && btb_hit) ? bht_item.msb | False
    val predict_is_jump_inst = btb_hit
    when(io.flush) {
        // predict fail, rescure
        ghr := io.updateBPU.payload.ghr(8 downto 0) @@ io.updateBPU.payload.jump
    }.elsewhen(io.toFetch.ready && io.toFetch.valid) {
        // predict, go on
        ghr := ghr(8 downto 0) @@ predict_jump
    }
    
    val update_bpu = io.updateBPU.valid
    val update_pc = io.updateBPU.payload.pc
    val update_predict_fail = io.updateBPU.payload.predict_fail
    val update_target = io.updateBPU.payload.target
    val update_jump = io.updateBPU.payload.jump
    val update_is_jump_inst = io.updateBPU.payload.is_jump_inst
    val update_ghr = io.updateBPU.payload.ghr
    val update_btb_index = update_pc(11 downto 2)
    val update_bht_index = update_pc(11 downto 2) ^ update_ghr
    val update_btb_item = ubtb.readAsync(update_btb_index)
    val update_bht_item = ubht.readAsync(update_bht_index)
    val update_btb_valid = btb_valid_list(update_btb_index)
    val update_bht_valid = bht_valid_list(update_bht_index)
    val update_btb_hit = update_btb_valid ? (update_btb_item.tag === update_pc(21 downto 12)) | False
    val update_bht_hit = update_bht_valid
    val btb_wen = False
    val bht_wen = False
    val btb_wdata = btbBundle().rst()
    val bht_wdata = U(0, 2 bits)
    val btb_windex = update_btb_index
    val bht_windex = update_bht_index
    when (update_bpu) {
        when(!update_btb_hit && !update_bht_hit) {
            when(update_jump && update_is_jump_inst) {
                // first time for this branch inst
                btb_wdata.tag := update_pc(21 downto 12)
                btb_wdata.target := update_target
                btb_wen := True
                bht_wdata := U"2'b10"
                bht_wen := True
                btb_valid_list(update_btb_index) := True
                bht_valid_list(update_bht_index) := True
            }.otherwise {
                // nothing
            }
        }.elsewhen(update_btb_hit && !update_bht_hit) {
            when(update_jump && update_is_jump_inst) {
                // first time for this branch path
                bht_wdata := U"2'b10"
                bht_wen := True
                bht_valid_list(update_bht_index) := True
            }.otherwise {
                // nothing
            }
        }.elsewhen(update_btb_hit && update_bht_hit) {
            when(update_is_jump_inst) {
                // only update bht
                bht_wen := True
                bht_wdata := update_jump ? (update_bht_item +| 1) | (update_bht_item -| 1)
            }.otherwise {
                // wrong hit
                // only update btb valid
                btb_valid_list(update_btb_index) := False
                bht_valid_list(update_bht_index) := False
            }
        }
    }
    pbtb.write(btb_windex, btb_wdata, btb_wen)
    ubtb.write(btb_windex, btb_wdata, btb_wen)
    pbht.write(bht_windex, bht_wdata, bht_wen)
    ubht.write(bht_windex, bht_wdata, bht_wen)

    when (io.flush) {
        pc := io.redirectPC
        pc_valid := True
    } .elsewhen(io.toFetch.ready && io.toFetch.valid) {
        pc := predict_jump ? predict_target | pc_plus_4
        pc_valid := True
    }
    
    io.toFetch.valid := pc_valid && !io.flush
    io.toFetch.payload.pc := pc
    io.toFetch.payload.bpuBundle := BPUBundle().set(predict_target, predict_jump, predict_is_jump_inst, ghr)
    
    io.updateBPU.ready := True
    
}
