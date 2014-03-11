type gpr = int
type fpr = int
type cc = int
type label = string
type offset = int

(* SPECIAL *)
type rdRsRt = [ `Add | `Addu | `Sub | `Subu | `And | `Or | `Nor | `Xor
			  | `Movz | `Movn | `Sllv | `Srav | `Srlv | `Slt | `Sltu ]
type rsRt = [ `Mult | `Div ]
type rsRtSa = [ `Sll | `Srl | `Sra ]
type rs = [ `Jr | `Mthi | `Mtlo ]
type rd = [ `Mfhi | `Mflo ]
type rdRs = [ `Jalr ]
type r0 = [ `Syscall | `Break ]

type special =
	RdRsRt of rdRsRt * gpr * gpr * gpr
  | RsRt of rsRt * gpr * gpr
  | RsRtSa of rsRtSa * gpr * gpr * int
  | Rs of rs * gpr
  | Rd of rd * gpr
  | RdRs of rdRs * gpr * gpr
  | R0 of r0

(* REGIMM *)
type regimm = [ `Bgez | `Bgezal | `Bltz | `Bltzal ]

let encode_regimm = function
  | `Bltz -> 0b00_000 | `Bgez -> 0b00_001
  | `Bltzal -> 0b10_000 | `Bgezal -> 0b10_001

(* Memory *)
type rtOffsetBase = [ `Lw | `Sw ]
type ftOffsetBase = [ `Lwc1 | `Swc1 ]

(* ALU *)
type rtRsImm = [ `Addi | `Addiu | `Andi | `Ori | `Slti | `Sltiu | `Xori ]
type rtImm = [ `Lui ]
type rsRtOffset = [ `Beq | `Bne ]
type rsOffset = [ `Bgtz | `Blez ]
type imm26 = [ `J | `Jal ]

(* FPU *)
type fmt = [ `Single | `Word ]
type fdFs = [ `Abs | `Mov | `Neg | `Recip | `Sqrt | `Cvt of fmt ]
type fdFsFt = [ `Add | `Sub | `Mul | `Div ]
type cond = F | Un | Eq | Ueq | Olt | Ult | Ole | Ule
type ccFsFt = [ `C of cond ]

type 'a cop1 =
	FdFs of fdFs * fmt * fpr * fpr
  | FdFsFt of fdFsFt * fmt * fpr * fpr * fpr
  | CcFsFt of ccFsFt * fmt * cc * fpr * fpr
  | TfCcOffset of [ `Bc ] * bool * cc * 'a
  | RtFs of [ `Mf | `Mt ] * gpr * fpr

(* IO *)
type cop2 =
	Rs of [ `Ob ] * gpr
  | Imm of [ `Obi ] * int
  | Rd of [ `Ib | `Ibu ] * gpr

(* Instructions *)
type 'a instruction =
	Special of special
  | Regimm  of regimm * gpr * 'a
  | RtOffsetBase of rtOffsetBase * gpr * int * gpr
  | FtOffsetBase of ftOffsetBase * fpr * int * gpr
  | RtRsImm of rtRsImm * gpr * gpr * int
  | RtImm of rtImm * gpr * int
  | RsRtOffset of rsRtOffset * gpr * gpr * 'a
  | RsOffset of rsOffset * gpr * 'a
  | Imm26 of imm26 * 'a
  | Cop1 of 'a cop1
  | Cop2 of cop2

(* Labeled Instructions and pseudo mnemonics *)
type mnemonic =
	Instruction of label instruction
  | Li of gpr * int
  | Li_s of fpr * float
  | Move of gpr * gpr
  | Nop

type directive =
	Mnemonic of mnemonic * int
  | Label of label * int
  | LabelMnemonic of label * mnemonic * int
  | Eof
