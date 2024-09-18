package hit_coa_lab

import spinal.core._
import spinal.lib._

case class BPUBundle() extends Bundle {
    val predict_target = UInt(32 bits)
    val predict_jump = Bool()
    val predict_is_jump_inst = Bool()
    val ghr = UInt(10 bits)
    
    def set(predict_target: UInt, predict_jump: Bool, predict_is_jump_inst: Bool, ghr: UInt): BPUBundle = {
        this.predict_target := predict_target
        this.predict_jump := predict_jump
        this.predict_is_jump_inst := predict_is_jump_inst
        this.ghr := ghr
        this
    }
    
    def rst(): BPUBundle = {
        this.predict_target := 0
        this.predict_jump := False
        this.predict_is_jump_inst := False
        this.ghr := 0
        this
    }
}

case class updateBPUBundle() extends Bundle {
    val pc = UInt(32 bits)
    val predict_fail = Bool()
    val target = UInt(32 bits)
    val jump = Bool()
    val is_jump_inst = Bool()
    val ghr = UInt(10 bits)
    
    def set(pc: UInt, predict_fail: Bool, target: UInt, jump: Bool, is_jump_inst: Bool, ghr: UInt): updateBPUBundle = {
        this.pc := pc
        this.predict_fail := predict_fail
        this.target := target
        this.jump := jump
        this.is_jump_inst := is_jump_inst
        this.ghr := ghr
        this
    }
    
    def rst(): updateBPUBundle = {
        this.pc := 0
        this.predict_fail := False
        this.target := 0
        this.jump := False
        this.is_jump_inst := False
        this.ghr := 0
        this
    }
}

case class btbBundle() extends Bundle {
    val tag = UInt(10 bits)
    val target = UInt(32 bits)
    
    def set(tag: UInt, target: UInt): btbBundle = {
        this.tag := tag
        this.target := target
        this
    }

    def rst(): btbBundle = {
        this.tag := 0
        this.target := 0
        this
    }
}
