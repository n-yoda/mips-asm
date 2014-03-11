%{
  open Instruction
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <int> GPR
%token <int> FPR
%token <int> LF

%token EOF X COLON L R

%token ADD ADDU SUB SUBU
%token MOVN MOVZ SLT SLTU
%token MULT DIV MFHI MFLO MTHI MTLO
%token AND NOR OR XOR
%token SLL SLLV SRA SRAV SRL SRLV
%token JALR JR SYSCALL BREAK

%token BGEZ BGEZAL BLTZ BLTZAL

%token ADD_S ABS_S DIV_S  MOV_S MUL_S
%token NEG_S RECIP_S SQRT_S SUB_S
%token CVT_S_W CVT_W_S
%token BC1F BC1T MFC1 MTC1
%token C_F_S C_UN_S C_EQ_S C_UEQ_S
%token C_OLT_S C_ULT_S C_OLE_S C_ULE_S

%token IB OB IBU OBI

%token ADDI ADDIU ANDI BEQ BGTZ BLEZ BNE J JAL
%token LUI LW LWC1 ORI SLTI SLTIU SW SWC1 XORI

%token LI LI_S MOVE NOP NEG
%token BGT BGE BLT BLE

%start directive
%type <Instruction.directive> directive

%%

directive:
  | ID COLON LF      { Label ($1, $3) }
  | ID mnemonic LF   { LabelMnemonic ($1, $2, $3) }
  | mnemonic LF      { Mnemonic ($1, $2) }
  | lf_eof           { Eof }
  | error            { Error }
;

lf_eof:
  | LF lf_eof { () }
  | EOF       { () }
;

mnemonic:
  | instruction          { Instruction $1 }
  | LI GPR X INT         { Li ($2, $4) }
  | LI_S FPR X FLOAT     { Li_s ($2, $4) }
  | MOVE GPR X GPR       { Move ($2, $4) }
  | NEG GPR X GPR        { Neg ($2, $4) }
  | cond2 GPR X GPR X ID { B ($1, $2, $4, $6) }
  | NOP                  { Nop }
;

instruction:
  | special         { Special $1 }
  | regimm GPR X ID { Regimm ($1, $2, $4) }
  | rtOffsetBase GPR X INT L GPR R { RtOffsetBase ($1, $2, $4, $6) }
  | ftOffsetBase FPR X INT L GPR R { FtOffsetBase ($1, $2, $4, $6) }
  | rtRsImm GPR X GPR X INT { RtRsImm ($1, $2, $4, $6) }
  | rtRsImm GPR X INT { RtRsImm ($1, $2, $2, $4) (* この形はアリ？ *) }
  | rtImm GPR X INT { RtImm ($1, $2, $4) }
  | rsRtOffset GPR X GPR X ID { RsRtOffset ($1, $2, $4, $6) }
  | rsOffset GPR X ID { RsOffset ($1, $2, $4) }
  | imm26 ID { Imm26 ($1, $2) }
  | cop1 { Cop1 $1 }
  | cop2 { Cop2 $1 }
;

special:
  | rdRsRt GPR X GPR X GPR  { RdRsRt ($1, $2, $4, $6) }
  | rsRt GPR X GPR          { RsRt ($1, $2, $4) }
  | rsRtSa GPR X GPR X INT  { RsRtSa ($1, $2, $4, $6) }
  | rs GPR                  { Rs ($1, $2) }
  | rd GPR                  { Rd ($1, $2) }
  | JALR GPR X GPR          { RdRs (`Jalr, $2, $4) }
  | JALR GPR                { RdRs (`Jalr, 31, $2) }
  | r0                      { R0 $1 }
;

cop1:
  | fdFs_s FPR X FPR         { FdFs ($1, `Single, $2, $4) }
  | fdFsFt_s FPR X FPR X FPR { FdFsFt ($1, `Single, $2, $4, $6) }
  | cond_s INT X FPR X FPR   { CcFsFt (`C $1, `Single, $2, $4, $6) }
  | cond_s FPR X FPR   { CcFsFt (`C $1, `Single, 0, $2, $4) }
  | fdFs_w FPR X FPR         { FdFs ($1, `Word, $2, $4) }
  | bc INT X ID    { TfCcOffset (`Bc, $1, $2, $4) }
  | bc ID          { TfCcOffset (`Bc, $1, 0, $2) }
  | MFC1 GPR X FPR { RtFs (`Mf, $2, $4) }
  | MTC1 GPR X FPR { RtFs (`Mt, $2, $4) }
;

cop2:
  | OB GPR  { Rs (`Ob, $2) }
  | OBI INT { Imm (`Obi, $2) }
  | IB GPR  { Rd (`Ib, $2) }
  | IBU GPR { Rd (`Ibu, $2) }
;

bc: | BC1F { false } | BC1T { true } ;

cond2: | BGT { Gt } | BGE { Ge } | BLT { Lt } | BLE { Le } ;

rdRsRt:
  | ADD { `Add } | ADDU { `Addu } | SUB { `Sub } | SUBU { `Subu }
  | AND { `And } | OR { `Or } | NOR { `Nor } | XOR { `Xor }
  | MOVZ { `Movz }  | MOVN { `Movn }
  | SLLV { `Sllv } | SRAV { `Srav } | SRLV { `Srlv }
  | SLT { `Slt } | SLTU { `Sltu }
;

rsRt: | MULT { `Mult } | DIV { `Div } ;

rsRtSa: | SLL { `Sll } | SRL { `Srl } | SRA { `Sra } ;

rs: | JR { `Jr } | MTHI { `Mthi } | MTLO { `Mtlo } ;

rd: | MFHI { `Mfhi } | MFLO { `Mflo } ;

r0: | SYSCALL { `Syscall } | BREAK { `Break } ;

regimm:
  | BGEZ { `Bgez } | BGEZAL { `Bgezal }
  | BLTZ { `Bltz } | BLTZAL { `Bltzal }
;

rtOffsetBase: | LW { `Lw } | SW { `Sw } ;

ftOffsetBase: | LWC1 { `Lwc1 } | SWC1 { `Swc1 } ;

rtRsImm:
  | ADDI { `Addi } | ADDIU { `Addiu } | ANDI { `Andi }
  | ORI { `Ori } | SLTI { `Slti } | SLTIU { `Sltiu } | XORI { `Xori }
;

rtImm: | LUI { `Lui } ;

rsRtOffset: | BEQ { `Beq } | BNE { `Bne } ;

rsOffset: | BGTZ { `Bgtz } | BLEZ { `Blez } ;

imm26: | J { `J } | JAL { `Jal } ;

fdFs_s:
  | ABS_S { `Abs } | MOV_S { `Mov } | NEG_S { `Neg }
  | RECIP_S { `Recip } | SQRT_S { `Sqrt } | CVT_W_S { `Cvt `Word }
;

fdFsFt_s: | ADD_S { `Add } | MUL_S { `Mul } | DIV_S { `Div } | SUB_S { `Sub } ;

fdFs_w: | CVT_S_W { `Cvt `Single } ;

cond_s:
  | C_F_S { F } | C_UN_S { Un } | C_EQ_S { Eq }
  | C_UEQ_S { Ueq } | C_OLT_S { Olt } | C_ULT_S { Ult }
  | C_OLE_S { Ole } | C_ULE_S { Ule }
;

%%
