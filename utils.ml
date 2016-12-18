(* Pretty Printer *)

open Ast
open Sast

let rec getType expr classEnv = match expr with
(*	  Id(s) -> getIdType s*)
	| Int_Lit(s) -> JInt
	| Float_Lit(f)	-> JFloat
	| Char_Lit(c)	-> JChar
	| String_Lit(s)	-> JString
	| Bool_Lit(b)	-> JBoolean
	| Binop(e1, op, e2) -> getType e1 classEnv
	| Unop(op, e)	-> getType e classEnv
	| Assign(s, e)	-> getType expr classEnv
(*	| FuncCall(s, el)	-> getFuncReturnType s env
*)

let addComma l = 
	let s = ref ""
	in
	let _ = List.iter (fun d -> s := !s ^ d ^ ",") l
	in
	String.sub !s 0 ((String.length !s) -1)
	

(* Print data types *)

let rec str_of_type = function
		JChar     -> "char"
	|	JVoid     -> "void"
	|	JBoolean  -> "boolean"
	|	JFloat    -> "float"
	| 	JInt 	  -> "int"
	| 	Object(s) -> "class " ^ s
	|	Arraytype(d, l) -> str_of_type d ^ "[]"
	|	Tuple(dl)	-> "tuple<" ^ addComma (List.map str_of_type dl) ^ ">"

let str_of_scope = function
		Public  -> "public"
	| 	Private -> "private"

let str_of_op = function
		Add	-> "+"
	|	Sub	-> "-"
        |     	Mult    -> "*"
        |      	Div     -> "/"
        |      	Equal   -> "=="
        |      	Neq     -> "!="
        |      	Less    -> "<"
        |      	Leq     -> "<="
        |      	Greater -> ">"
        |      	Geq	-> ">="
        |      	And     -> "and"
        |      	Not     -> "!"
        |      	Or      -> "or"

(* Pretty Printing for Ast *)

let rec str_of_expr expr = match expr with
	  Int_Lit(i)		-> string_of_int i
	| Bool_Lit(b)		-> if b then "true" else "false"
	| Float_Lit(f)		-> string_of_float f
	| String_Lit(s)		-> s
	| Char_Lit(c)		-> Char.escaped c
	| Null			-> "null"
	| Id(s)			-> s
	| Binop(e1, op, e2)	-> "" ^ str_of_expr e1 ^ " " ^ str_of_op op ^ " " ^ str_of_expr e2
	| Assign(e1, e2)		-> "" ^ str_of_expr e1 ^ " = " ^ str_of_expr e2
	| Noexpr		-> ""
	| FuncCall(s, el)	-> "" ^ s ^ "(" ^ addComma (List.map str_of_expr el) ^ ")"
	| Unop(op, e)		-> "" ^ str_of_op op ^ " " ^ str_of_expr e
 
(* Pretty Printing for Sast *)

let appendList h t = match t with
	  []	-> ""
	| _	-> "" ^ (List.fold_left (fun hd tl -> hd^tl) h t)

let get_str l f = 
	if List.length l = 0 then ""
	else if List.length l = 1 then f (List.hd l)
	else begin
		print_endline("run");
		let strList = List.map f l in
		let h = List.hd strList
		and t = List.tl strList 
		in
		appendList h t
	end


let str_of_svdecl svdecl =
	"" ^ str_of_scope svdecl.svscope ^ " " ^ str_of_type svdecl.svtype ^ " " ^ svdecl.svname ^ ";\n"

let str_of_svariables variables =
	get_str variables str_of_svdecl

let rec str_of_sexpr expr = match expr with
	  SInt_Lit(i)		-> string_of_int i
	| SBoolean_Lit(b)	-> if b then "true" else "false"
	| SFloat_Lit(f)		-> string_of_float f
	| SString_Lit(s)	-> s
	| SChar_Lit(c)		-> Char.escaped c
	| SNull			-> "null"
	| SId(s,_)		-> s
	| SBinop(e1, op, e2, _)	-> "" ^ str_of_sexpr e1 ^ " " ^ str_of_op op ^ " " ^ str_of_sexpr e2
	| SAssign(e1, e2, _)	-> "" ^ str_of_sexpr e1 ^ " = " ^ str_of_sexpr e2
	| SNoexpr		-> ""
	| SFuncCall(s, el, _, _)->  "" ^ s ^ "(" ^ addComma (List.map str_of_sexpr el) ^ ")"
	| SUnop(op, e, _)	-> "" ^ str_of_op op ^ " " ^ str_of_sexpr e
 
let rec str_of_sstmt stmt = match stmt with
	  SBlock(sl)	-> "" ^ (get_str sl str_of_sstmt)
	| SExpr(e,_)	-> "" ^ str_of_sexpr e ^ ";\n"
	| SVarDecl(svdecl)	-> "" ^ str_of_svdecl svdecl
	| SReturn(e, _) 	-> "return " ^ str_of_sexpr e ^ ";\n"   
	| SIf(e, s1, s2)	-> "if(" ^ str_of_sexpr e ^ ") {\n" ^ str_of_sstmt s1 ^ "} else {\n" ^ str_of_sstmt s2 ^ "}\n"
	| SFor(e1, e2, e3, s)	-> "for(" ^ str_of_sexpr e1 ^ "; " ^ str_of_sexpr e2 ^ "; " ^ str_of_sexpr e3 ^ ") {\n" ^ str_of_sstmt s ^ "}\n"
	| SWhile(e, s)		-> "while(" ^ str_of_sexpr e ^ ") {\n" ^ str_of_sstmt s ^ "}\n"
	
let str_of_sfbody fbody = get_str fbody str_of_sstmt

let str_of_sformal formal = 
	"" ^ str_of_type formal.sformal_type ^ " " ^ formal.sformal_name
 
let str_of_sformals formals = get_str formals str_of_sformal


let str_of_sfunction fdecl =
	"" ^ str_of_scope fdecl.sfscope ^ " "^ str_of_type fdecl.sfreturn ^ " " ^ fdecl.sfname ^ " (" ^
		str_of_sformals fdecl.sfformals ^ ") {\n" ^ str_of_sfbody fdecl.sfbody ^ "}"

let str_of_sconstructor constructor =
	"" ^ str_of_sfunction constructor

let str_of_sconstructors constructors = get_str constructors str_of_sconstructor 

let str_of_smethods methods = get_str methods str_of_sfunction
 
let str_of_scbody cbody = 
	"" ^ str_of_svariables cbody.svariables ^ str_of_sconstructors cbody.sconstructors ^ str_of_smethods cbody.smethods

let str_of_sclass c = 
	"" ^ str_of_scope c.scscope ^ " " ^ c.scname ^ " {\n" ^ str_of_scbody c.scbody ^ "\n}\n"

let str_of_sclasses scdecls = get_str scdecls str_of_sclass

let print_str_of_sast classes = print_string (str_of_sclasses classes)

