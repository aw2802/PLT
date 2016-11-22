(* Pretty Printer *)

open Sast
open Ast
open Semant

(* let save file string =
	let channel 

*)

(* Print data types *)

let str_of_primitive = function
		JChar     -> "char"
	|	JVoid     -> "void"
	|	JBoolean  -> "boolean"
	|	JFloat    -> "float"
	| 	JInt 	  -> "int"
	| 	Object(s) -> "class " ^ s

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


let str_of_vdecl svdecl =
	"" ^ str_of_scope svdecl.svscope ^ " " ^ str_of_primitive svdecl.svtype ^ " " ^ svdecl.svname ^ ";\n"

let str_of_variables variables =
	get_str variables str_of_vdecl

let rec str_of_expr expr = match expr with
	  SInt_Lit(i)		-> string_of_int i
	| SBoolean_Lit(b)	-> if b then "true" else "false"
	| SFloat_Lit(f)		-> string_of_float f
	| SString_Lit(s)	-> s
	| SChar_Lit(c)		-> Char.escaped c
	| SNull			-> "null"
	| SId(s,_)		-> s
	| SBinop(e1, op, e2, _)	-> "" ^ str_of_expr e1 ^ " " ^ str_of_op op ^ " " ^ str_of_expr e2
	| SAssign(e1, e2, _)	-> "" ^ str_of_expr e1 ^ " = " ^ str_of_expr e2
	| SNoexpr		-> ""
	| SFuncCall(s, el, _, _)->  "" ^ s ^ "(" ^ (get_str el str_of_expr) ^ ")"
	| SUnop(op, e, _)	-> "" ^ str_of_op op ^ " " ^ str_of_expr e
 
let rec str_of_stmt stmt = match stmt with
	  SBlock(sl)	-> "" ^ (get_str sl str_of_stmt)
	| SExpr(e,_)	-> "" ^ str_of_expr e ^ ";\n"
	| SVarDecl(vdecl, _,_)	-> "" ^ str_of_vdecl vdecl
	| SReturn(e, _) 	-> "return " ^ str_of_expr e ^ ";\n"   
	| SIf(e, s1, s2)	-> "if(" ^ str_of_expr e ^ ") {\n" ^ str_of_stmt s1 ^ "} else {\n" ^ str_of_stmt s2 ^ "}\n"
	| SFor(e1, e2, e3, s)	-> "for(" ^ str_of_expr e1 ^ "; " ^ str_of_expr e2 ^ "; " ^ str_of_expr e3 ^ ") {\n" ^ str_of_stmt s ^ "}\n"
	| SWhile(e, s)		-> "while(" ^ str_of_expr e ^ ") {\n" ^ str_of_stmt s ^ "}\n"
	
let str_of_fbody fbody = get_str fbody str_of_stmt

let str_of_formal formal = 
	"" ^ str_of_primitive formal.sformal_type ^ " " ^ formal.sformal_name
 
let str_of_formals formals = get_str formals str_of_formal


let str_of_function fdecl =
	"" ^ str_of_scope fdecl.sfscope ^ " "^ str_of_primitive fdecl.sfreturn ^ " " ^ fdecl.sfname ^ " (" ^
		str_of_formals fdecl.sfformals ^ ") {\n" ^ str_of_fbody fdecl.sfbody ^ "}"

let str_of_constructor constructor =
	"" ^ str_of_function constructor

let str_of_constructors constructors = get_str constructors str_of_constructor 

let str_of_methods methods = get_str methods str_of_function
 
let str_of_cbody cbody = 
	"" ^ str_of_variables cbody.svariables ^ str_of_constructors cbody.sconstructors ^ str_of_methods cbody.smethods

let str_of_class c = 
	"" ^ str_of_scope c.scscope ^ " " ^ c.scname ^ " {\n" ^ str_of_cbody c.scbody ^ "\n}\n"

let str_of_classes scdecls = get_str scdecls str_of_class

let print_str_of_sast classes = print_string (str_of_classes classes)
