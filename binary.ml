open Instruction

let encode_op = function
  | `Special -> 0b000_000 | `Regimm -> 0b000_001
  | `J -> 0b000_010 | `Jal -> 0b000_011
  | `Beq -> 0b000_100 | `Bne -> 0b000_101
  | `Blez -> 0b000_110 | `Bgtz -> 0b000_111
  | `Addi -> 0b001_000 | `Addiu -> 0b001_001
  | `Slti -> 0b001_010 | `Sltiu -> 0b001_011
  | `Andi -> 0b001_100 | `Ori -> 0b001_101
  | `Xori -> 0b001_110 | `Lui -> 0b001_111
  | `Cop0 -> 0b010_000 | `Cop1 -> 0b010_001
  | `Cop2 -> 0b010_010 | `Cop1x -> 0b010_011
  | `Lw -> 0b100_011
  | `Sw -> 0b101_011
  | `Lwc1 -> 0b110_001
  | `Swc1 -> 0b111_001

let encode_sp = function
  | `Sll -> 0b000_000
  | `Srl -> 0b000_010 | `Sra -> 0b000_011
  | `Sllv -> 0b000_100
  | `Srlv -> 0b000_110 | `Srav -> 0b000_111
  | `Jr -> 0b001_000 | `Jalr -> 0b001_001
  | `Movz -> 0b001_010 | `Movn -> 0b001_011
  | `Syscall -> 0b001_100 | `Break -> 0b001_101
  | `Mfhi -> 0b010_000 | `Mthi -> 0b010_001
  | `Mflo -> 0b010_010 | `Mtlo -> 0b010_011
  | `Mult -> 0b011_000 | `Multu -> 0b011_001
  | `Div -> 0b011_010 | `Divu -> 0b011_011
  | `Add -> 0b100_000 | `Addu -> 0b100_001
  | `Sub -> 0b100_010 | `Subu -> 0b100_011
  | `And -> 0b100_100 | `Or -> 0b100_101
  | `Xor -> 0b100_110 | `Nor -> 0b100_111
  | `Slt -> 0b101_010| `Sltu -> 0b101_011

let encode_io = function
  | `Ib -> 0b00011
  | `Ob -> 0b10011
  | `Ibu -> 0b01011
  | `Obi -> 0b10111

let encode_fmt = function
  | `Single -> 0b10000
  | `Word -> 0b10100
  | `Bc -> 0b01000
  | `Mf -> 0b00000
  | `Mt -> 0b00100

let encode_fmt3 = function
  | `Single -> 0b000
  | `Word -> 0b100

let encode_cond = function
  | F -> 0b0000 | Un -> 0b0001 | Eq -> 0b0010 | Ueq -> 0b0011
  | Olt -> 0b0100 | Ult -> 0b0101 | Ole -> 0b0110 | Ule -> 0b0111

let encode_fpu = function
  | `Add -> 0b000_000 | `Sub -> 0b000_001
  | `Mul -> 0b000_010 | `Div -> 0b000_011
  | `Sqrt -> 0b000_100 | `Abs -> 0b000_101
  | `Mov -> 0b000_110 | `Neg -> 0b000_111
  | `Recip -> 0b010101
  | `Cvt x -> 0b100_000 lor (encode_fmt3 x)
  | `C x -> 0b11_0000 lor (encode_cond x)
  | `None -> 0

let _5_5_5_5_6 a b c d e =
  ((a land 0b11111) lsl 21)
  lor ((b land 0b11111) lsl 16)
  lor ((c land 0b11111) lsl 11)
  lor ((d land 0b11111) lsl 6)
  lor (e land 0b111111)

let _6_5_5_16 a b c d =
  ((a land 0b111111) lsl 26)
  lor ((a land 0b11111) lsl 21)
  lor ((b land 0b11111) lsl 16)
  lor (c land 0xffff)

let _5_5_5_5_sp a b c d sp = _5_5_5_5_6 a b c d (encode_sp sp)

let fmt_5_5_5_fpu fmt a b c fpu =
  _5_5_5_5_6 (encode_fmt fmt) a b c (encode_fpu fpu)

let fmt_3_1_1_16 fmt a b c d =
  let abc =
	((a land 0b111) lsl 2) lor ((b land 1) lsl 1) lor (c land 1)
  in _6_5_5_16 0 (encode_fmt fmt) abc d

let _5_io_5 a io b = _5_5_5_5_6 a (encode_io io) b 0 0

let _5_io_16 a io b = _6_5_5_16 0 a (encode_io io) b

let op_26 op a =
  ((encode_op op) lsl 26) lor (a land 0b11_1111_1111_1111_1111_1111_1111)

let op_5_5_16 op a b c = _6_5_5_16 (encode_op op) a b c

let encode_special = function
  | RdRsRt (sp, rd, rs, rt) -> _5_5_5_5_sp rs rt rd 0 sp
  | RsRt (sp, rs, rt) -> _5_5_5_5_sp rs rt 0 0 sp
  | RsRtSa (sp, rs, rt, sa) -> _5_5_5_5_sp rs rt 0 sa sp
  | Rs (sp, rs) -> _5_5_5_5_sp rs 0 0 0 sp
  | Rd (sp, rd) -> _5_5_5_5_sp 0 0 rd 0 sp
  | RdRs (sp, rd, rs) -> _5_5_5_5_sp rs 0 rd 0 sp
  | R0 sp -> _5_5_5_5_sp 0 0 0 0 sp

let encode_cop1 = function
  | FdFs (fpu, fmt, fd, fs) -> fmt_5_5_5_fpu fmt 0 fs fd fpu
  | FdFsFt (fpu, fmt, fd, fs, ft) -> fmt_5_5_5_fpu fmt ft fs fd fpu
  | CcFsFt (fpu, fmt, cc, fs, ft) -> fmt_5_5_5_fpu fmt ft fs (cc lsl 2) fpu
  | TfCcOffset (fmt, tf, cc, off) -> fmt_3_1_1_16 fmt cc 0 (if tf then 1 else 0) off
  | RtFs (fmt, rt, fs) -> fmt_5_5_5_fpu fmt rt fs 0 `None

let encode_cop2 = function
  | Rs (io, rs) -> _5_io_5 rs io 0
  | Imm (io, imm) -> _5_io_16 0 io imm
  | Rd (io, rd) -> _5_io_5 0 io rd

let encode = function
  | Special sp -> op_26 `Special (encode_special sp)
  | Cop1 x -> op_26 `Cop1 (encode_cop1 x)
  | Cop2 x -> op_26 `Cop2 (encode_cop2 x)
  | Regimm (ri, rs, off) -> op_5_5_16 `Regimm rs (encode_regimm ri) off
  | RtOffsetBase (op, rt, off, rs) -> op_5_5_16 op rs rt off
  | FtOffsetBase (op, ft, off, rs) -> op_5_5_16 op rs ft off
  | RtRsImm (op, rt, rs, imm) -> op_5_5_16 op rs rt imm
  | RtImm (op, rt, imm) -> op_5_5_16 op 0 rt imm
  | RsRtOffset (op, rs, rt, off) -> op_5_5_16 op rs rt off
  | RsOffset (op, rs, off) -> op_5_5_16 op rs 0 off
  | Imm26 (op, off) -> op_26 op off
