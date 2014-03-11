open Instruction
open Binary

let _ = if Sys.word_size < 33 then (print_string "32bit CPU is not supported.\n"; exit 1)

(* 擬似ニーモニックを命令列に変換 *)
let rec insts_of_mnemonic mne =
  match mne with
	| Instruction inst -> [inst]
	| Li (gpr, imm) ->
	  (match (imm lsl 15) land 0b1111_1111_1111_1111_1 with
		| 0b1111_1111_1111_1111_1 -> [RtRsImm (`Addi, gpr, 0, imm land 0xffff)]
		| 0 | 1 -> [RtRsImm (`Ori, gpr, 0, imm land 0xffff)]
		| _ -> [RtImm (`Lui, gpr, (imm lsr 16) land 0xffff);
				RtRsImm (`Ori, gpr, 0, imm land 0xffff)])
	| Li_s (fpr, imm) ->
	  let bits = Int32.to_int (Int32.bits_of_float imm) in
	  let i = insts_of_mnemonic (Li (1, bits)) in
	  List.append i [Cop1 (RtFs (`Mt, 1, fpr))]
	| Move (rd, rs) -> [Special (RdRsRt (`Or, rd, rs, 0))]
	| Neg (rd, rt) -> [Special (RdRsRt (`Sub, rd, 0, rt))]
	| Nop -> [Special (RsRtSa (`Sll, 0, 0, 0))]
	| B (Gt, rs, rt, lbl) ->
	  [Special (RdRsRt (`Slt, 1, rt, rs)); RsRtOffset (`Bne, 1, 0, lbl)]
	| B (Lt, rs, rt, lbl) ->
	  [Special (RdRsRt (`Slt, 1, rs, rt)); RsRtOffset (`Bne, 1, 0, lbl)]
	| B (Ge, rs, rt, lbl) ->
	  [Special (RdRsRt (`Slt, 1, rs, rt)); RsRtOffset (`Beq, 1, 0, lbl)]
	| B (Le, rs, rt, lbl) ->
	  [Special (RdRsRt (`Slt, 1, rt, rs)); RsRtOffset (`Beq, 1, 0, lbl)]

(* append (reverse a) b *)
let rec rappend a line b =
  match a with
	| [] -> b
	| x::a -> rappend a line ((x, line)::b)

(* directive listを逆向きinstruction listに変換 *)
let rec parse lexbuf insts count tbl =
  match Parser.directive Lexer.token lexbuf with
	| Eof -> (insts, count)
	| Error -> failwith (string_of_int count)
	| Label (label,line) ->
	  (Hashtbl.add tbl label count;
	   parse lexbuf insts count tbl)
	| Mnemonic (mne, line) ->
	  let insts = (rappend (insts_of_mnemonic mne) line insts) in
	  parse lexbuf insts (count + 1) tbl
	| LabelMnemonic (label, mne, line) ->
	  let insts = (rappend (insts_of_mnemonic mne) line insts) in
	  (Hashtbl.add tbl label count;
	   parse lexbuf insts (count + 1) tbl)

(* 逆向きlabel instruction listを、int instruction listに変換*)
let rec unlabel pre post addr tbl =
  let get label = Hashtbl.find tbl label in
  let relative addr label =
	let target = (get label) - addr in
	if (abs target) < (1 lsl 15) then target
	else failwith "bad jump." in
  let absolute addr label =
	let target = get label in
	if (target lsr 26) = (addr lsr 26) then target land (1 lsl 26 - 1)
	else failwith "bad jump." in
  match pre with
	| [] -> post
	| (x, l)::pre ->
	  let next x' =
		unlabel pre ((x', l)::post) (addr - 1) tbl in
	  next (match x with
		| Regimm (ri, gpr, lbl) -> Regimm (ri, gpr, relative addr lbl)
		| Cop1 x -> Cop1 (match x with
			| TfCcOffset (bc, tf, cc, lbl) ->
			  TfCcOffset (bc, tf, cc, relative addr lbl)
			| FdFs (x, y, z, w) -> FdFs (x, y, z, w)
			| FdFsFt (x, y, z, w, v) -> FdFsFt (x, y, z, w, v)
			| CcFsFt (x, y, z, w, v) -> CcFsFt (x, y, z, w, v)
			| RtFs (x, y, z) -> RtFs (x, y, z))
		| RsRtOffset (op, rs, rt, lbl) -> RsRtOffset (op, rs, rt, relative addr lbl)
		| RsOffset (op, rs, lbl) -> RsOffset (op, rs, relative addr lbl)
		| Imm26 (j, lbl) -> Imm26 (j, absolute addr lbl)
		| Special x -> Special x
		| RtOffsetBase (a,b,c,d) -> RtOffsetBase (a,b,c,d)
		| FtOffsetBase (a,b,c,d) -> FtOffsetBase (a,b,c,d)
		| RtRsImm (a,b,c,d) -> RtRsImm (a,b,c,d)
		| RtImm (a,b,c) -> RtImm (a,b,c)
		| Cop2 a -> Cop2 a)

(* lexbuf *)
let lexbuf = Lexing.from_channel stdin

(* ラベルテーブル *)
let labels = Hashtbl.create 0

(* パース *)
let (rev, count) = parse lexbuf [] 0 labels

(* ラベル解決 *)
let insts = unlabel rev [] (count - 1) labels

let print_bit_32 x =
  for i = 0 to 31 do
	print_char (if x land (0x80_00_00_00 lsr i) = 0 then '0' else '1')
  done

(* 機械語に変換 *)
let _ = List.iter (fun x ->
  print_bit_32 (encode (fst x)); print_newline ()) insts

