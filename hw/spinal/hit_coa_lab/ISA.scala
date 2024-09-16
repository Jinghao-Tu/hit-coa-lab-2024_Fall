package hit_coa_lab

import spinal.core._
import spinal.lib._

object OP extends SpinalEnum {
    val DEFAULT, ADD, SUB, AND, OR, XOR, SW, LW, J, MOVZ, SLL, CMP, BBT = newElement()
}

object INST {
    def ADD = M"000000---------------00000100000"
    def SUB = M"000000---------------00000100010"
    def AND = M"000000---------------00000100100"
    def OR  = M"000000---------------00000100101"
    def XOR = M"000000---------------00000100110"
    def SW  = M"101011--------------------------"
    def LW  = M"100011--------------------------"
    def J   = M"000010--------------------------"
    def MOVZ= M"000000---------------00000001010"
    def SLL = M"00000000000---------------000000"
    def CMP = M"111110---------------00000000000"
    def BBT = M"111111--------------------------"
}

