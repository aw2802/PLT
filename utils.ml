(* Pretty Printer *)

open Ast
(* let save file string =
	let channel 

*)

(* Print data types *)

let str_of_primitives = function
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

let str_of_vname = function
		Id(s) -> s;
(**
let str_of_vdecl =
	"" ^ str_of_scope vdecl.scope ^ " " ^ str_of_primitives vdecl.vtype ^ " " ^ str_of_vname vdecl.name **)
