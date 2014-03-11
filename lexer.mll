{
  open Parser
  open String

  let line = ref 0

  let rec count s c i =
	if i < String.length s
	then (if s.[i] = c then 1 else 0) + (count s c (i + 1))
	else 0
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let symbol = '.' | '-' | '_'
let nondigit = alpha | symbol
let numeric = digit | alpha | '_'
let id = nondigit (digit | nondigit)*
let sp = [' ' '\t']
let ss = sp* ":" sp* | sp+

let A = ['A' 'a']
let B = ['B' 'b']
let C = ['C' 'c']
let D = ['D' 'd']
let E = ['E' 'e']
let F = ['F' 'f']
let G = ['G' 'g']
let H = ['H' 'h']
let I = ['I' 'i']
let J = ['J' 'j']
let K = ['K' 'k']
let L = ['L' 'l']
let M = ['M' 'm']
let N = ['N' 'n']
let O = ['O' 'o']
let P = ['P' 'p']
let Q = ['Q' 'q']
let R = ['R' 'r']
let S = ['S' 's']
let T = ['T' 't']
let U = ['U' 'u']
let V = ['V' 'v']
let W = ['W' 'w']
let X = ['X' 'x']
let Y = ['Y' 'y']
let Z = ['Z' 'z']

rule token = parse
  | sp* "#"               { comment lexbuf }
  | (sp* "\n")+ as s      { line := !line + (count s '\n' 0);
							LF (!line - 1) }
  | ss A D D sp*          { ADD }
  | ss A D D U sp*        { ADDU }
  | ss A N D sp*          { AND }
  | ss D I V sp*          { DIV }
  | ss J A L R sp*        { JALR }
  | ss J R sp*            { JR }
  | ss M F H I sp*        { MFHI }
  | ss M F L O sp*        { MFLO }
  | ss M O V N sp*        { MOVN }
  | ss M O V Z sp*        { MOVZ }
  | ss M T H I sp*        { MTHI }
  | ss M T L O sp*        { MTLO }
  | ss M U L T sp*        { MULT }
  | ss N O R sp*          { NOR }
  | ss O R sp*            { OR }
  | ss S L L sp*          { SLL }
  | ss S L L V sp*        { SLLV }
  | ss S L T sp*          { SLT }
  | ss S L T U sp*        { SLTU }
  | ss S R A sp*          { SRA }
  | ss S R A V sp*        { SRAV }
  | ss S R L sp*          { SRL }
  | ss S R L V sp*        { SRLV }
  | ss S U B sp*          { SUB }
  | ss S U B U sp*        { SUBU }
  | ss S Y S C A L L sp*  { SYSCALL }
  | ss X O R sp*          { XOR }
  | ss B G E Z sp*        { BGEZ }
  | ss B G E Z A L sp*    { BGEZAL }
  | ss B L T Z sp*        { BLTZ }
  | ss B L T Z A L sp*    { BLTZAL }
  | ss A B S '.' S sp*        { ABS_S }
  | ss A D D '.' S sp*        { ADD_S }
  | ss C V T '.' S '.' W sp*  { CVT_S_W }
  | ss C V T '.' W '.' S sp*  { CVT_W_S }
  | ss M O V '.' S sp*        { MOV_S }
  | ss M U L '.' S sp*        { MUL_S }
  | ss N E G '.' S sp*        { NEG_S }
  | ss R E C I P '.' S sp*    { RECIP_S }
  | ss S Q R T '.' S sp*      { SQRT_S }
  | ss S U B '.' S sp*        { SUB_S }
  | ss B C '1' F sp*          { BC1F }
  | ss B C '1' T sp*          { BC1T }
  | ss M F C '1' sp*          { MFC1 }
  | ss M T C '1' sp*          { MTC1 }
  | ss C '.' F '.' S sp*      { C_F_S }
  | ss C '.' U N '.' S sp*    { C_UN_S }
  | ss C '.' E Q '.' S sp*    { C_EQ_S }
  | ss C '.' U E Q '.' S sp*  { C_UEQ_S }
  | ss C '.' O L T '.' S sp*  { C_OLT_S }
  | ss C '.' U L T '.' S sp*  { C_ULT_S }
  | ss C '.' O L E '.' S sp*  { C_OLE_S }
  | ss C '.' U L E '.' S sp*  { C_ULE_S }
  | ss I B sp*                { IB }
  | ss O B sp*                { OB }
  | ss I B U sp*              { IBU }
  | ss O B I sp*              { OBI }
  | ss A D D I sp*            { ADDI }
  | ss A D D I U sp*          { ADDIU }
  | ss A N D I sp*            { ANDI }
  | ss B E Q sp*              { BEQ }
  | ss B G T Z sp*            { BGTZ }
  | ss B L E Z sp*            { BLEZ }
  | ss B N E sp*              { BNE }
  | ss J sp*                  { J }
  | ss J A L sp*              { JAL }
  | ss L U I sp*              { LUI }
  | ss L W sp*                { LW }
  | ss L W C '1' sp*          { LWC1 }
  | ss O R I sp*              { ORI }
  | ss S L T I sp*            { SLTI }
  | ss S L T I U sp*          { SLTIU }
  | ss S W sp*                { SW }
  | ss S W C '1' sp*          { SWC1 }
  | ss B R E A K sp*          { BREAK }
  | ss L I sp*                { LI }
  | ss L I '.' S sp*          { LI_S }
  | ss M O V E sp*            { MOVE }
  | ss N O P sp*              { NOP }
  | sp* ":"                   { COLON }
  | sp* "," sp*               { X }
  | sp* "(" sp*               { L }
  | sp* ")" sp*               { R }
  | id as s                   { ID s }
  | digit+ as d               { INT (int_of_string d) }
  | '0' numeric+ as d         { INT (int_of_string d) }
  | digit+ '.' digit* as d    { FLOAT (float_of_string d) }
  | digit* '.' digit+ as d    { FLOAT (float_of_string d) }
  | '0' numeric+ as d         { INT (int_of_string d) }
  | '\'' _ '\'' as s          { INT (int_of_char s.[1]) }
  | "$f"digit+ as reg     { FPR (int_of_string (sub reg 2 (length reg - 2))) }
  | "$"digit+ as reg      { GPR (int_of_string (sub reg 1 (length reg - 1))) }
  | "$zero"               { GPR 0 }
  | "$at"                 { GPR 1 }
  | "$v0"                 { GPR 2 }
  | "$v1"                 { GPR 3 }
  | "$a0"                 { GPR 4 }
  | "$a1"                 { GPR 5 }
  | "$a2"                 { GPR 6 }
  | "$a3"                 { GPR 7 }
  | "$t0"                 { GPR 8 }
  | "$t1"                 { GPR 9 }
  | "$t2"                 { GPR 10 }
  | "$t3"                 { GPR 11 }
  | "$t4"                 { GPR 12 }
  | "$t5"                 { GPR 13 }
  | "$t6"                 { GPR 14 }
  | "$t7"                 { GPR 15 }
  | "$s0"                 { GPR 16 }
  | "$s1"                 { GPR 17 }
  | "$s2"                 { GPR 18 }
  | "$s3"                 { GPR 19 }
  | "$s4"                 { GPR 20 }
  | "$s5"                 { GPR 21 }
  | "$s6"                 { GPR 22 }
  | "$s7"                 { GPR 23 }
  | "$t8"                 { GPR 24 }
  | "$t9"                 { GPR 25 }
  | "$k0"                 { GPR 26 }
  | "$k1"                 { GPR 27 }
  | "$gp"                 { GPR 28 }
  | "$sp"                 { GPR 29 }
  | "$fp"                 { GPR 30 }
  | "$ra"                 { GPR 31 }
  | sp* eof               { EOF }

and comment = parse
  | '\n'    { token lexbuf }
  | _       { comment lexbuf }
