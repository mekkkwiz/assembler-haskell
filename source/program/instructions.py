inst = {
    "name"      : ["add", "nand", "lw", "sw", "beq", "jalr", "halt", "noop"],
    "type"      : ["R", "R", "I", "I", "I", "J", "O", "O"],
    "optc_bin"  : ["000", "001", "010", "011", "100", "101", "110", "111"]
}

'''
        R-type instructions (add, nand)

                Bits 24-22 opcode

                Bits 21-19 reg A (rs)

                Bits 18-16 res B (rt)

                Bits 15-3 ไม่ใช้ (ควรตั้งไว้ที่ 0)

                Bits 2-0  destReg (rd)

        I-type instructions (lw, sw, beq)

                Bits 24-22 opcode

                Bits 21-19 reg A (rs)

                Bits 18-16 reg B (rt)

                Bits 15-0 offsetField (เลข16-bit และเป็น 2’s complement  โดยอยู่ในช่วง –32768 ถึง 32767)

        J-Type instructions (jalr)

                Bits 24-22 opcode

                Bits 21-19 reg A (rs)

                Bits 18-16 reg B (rd)

                Bits 15-0 ไม่ใช้ (ควรตั้งไว้ที่ 0)

        O-type instructions (halt, noop)

                Bits 24-22 opcode

                Bits 21-0 ไม่ใช้ (ควรตั้งไว้ที่ 0)
'''