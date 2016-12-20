(* Pretty Printer *)

open Ast
open Sast

module StringMap = Map.Make(String)

type methodSignature = {
        mscope: scope;
        mname : string;
        mformalTypes: data_type list;
	mReturn: data_type;
}

type classMap = {
        mutable variableMap: Ast.vdecl StringMap.t;
        mutable constructorMap: methodSignature StringMap.t;
        mutable methodMap:  methodSignature StringMap.t;
}

type classEnv = {
 	mutable className: string;
 	mutable classMaps: classMap StringMap.t;
 	mutable classMap: classMap;
	builtinMethods: string list;
}

type env = {
        mutable envClassName: string;
        mutable envClassMaps: classMap StringMap.t;
        mutable envClassMap: classMap;
        mutable envLocals: data_type StringMap.t;
        mutable envParams: data_type StringMap.t;
        mutable envReturnType: data_type;
	envBuiltinMethods: string list;
}

let getListOfFormalTypes c = 
	List.map (fun f -> f.fvtype) c.fformals

let getIdType s env =
	let checking =
	if StringMap.mem s env.envLocals
		then begin
			try (StringMap.find s env.envLocals)
			with Not_found -> raise(Failure("Unfound local")) 
		end
	else if StringMap.mem s env.envParams
		then try (StringMap.find s env.envParams)
		with Not_found -> raise(Failure("Unfound param"))
	else if StringMap.mem s env.envClassMap.variableMap
                then begin
			try (StringMap.find s env.envClassMap.variableMap).vtype
			with Not_found -> raise(Failure("Unfound class variable"))
		end
	else if StringMap.mem s env.envClassMaps
		then Object(s)
	else raise(Failure("Undeclared identifier " ^ s))
	in checking

let getFuncType s fl env =
	if List.mem s env.envBuiltinMethods
		then JVoid
	else begin
		let updated = ref false
		and
		t = ref JVoid
		in
		StringMap.iter (fun _ v -> if v.mformalTypes=fl 
			then (t:=v.mReturn; updated:=true)) 
			(StringMap.filter (fun k _-> k=s) env.envClassMap.methodMap);
		if !updated = true 
			then !t
		else raise(Failure("Undeclared method " ^ s))
	end

let rec getType expr env = match expr with
	  Id(s) 		-> getIdType s env
	| Int_Lit(s) 		-> JInt
	| Float_Lit(f)		-> JFloat
	| Char_Lit(c)		-> JChar
	| String_Lit(s)		-> Object("String")
	| Bool_Lit(b)		-> JBoolean
	| Noexpr		-> JVoid
	| Null			-> JVoid
	| Binop(e1, op, e2) 	-> (match op with
					  Equal|Neq|Less|Leq|Greater|Geq|Or|And|Not -> JBoolean
					| Add|Sub|Mult|Div -> if ((getType e1 env)=JFloat) || ((getType e2 env)=JFloat)
								then JFloat
								else JInt)
	| Unop(op, e)		-> getType e env
	| Assign(s, e)		-> getType e env
	| TupleCreate(dl, el) 	-> Tuple(dl)
	| TupleAccess(e1, e2) 	-> getType e1 env
	| ObjAccess(e1, e2)	-> getType e1 env
	| CreateObject(s, el) 	-> getType (Id(s)) env
	| ArrayCreate(d, el)	-> Arraytype(d, List.length el)
	| ArrayAccess(e, el)	-> getType e env  
	| FuncCall(s, el)	-> getFuncType s (List.map (fun e -> getType e env) el) env


let addComma l =
	if l = [] then ""
	else begin
		let s = ref ""
		in
		let _ = List.iter (fun d -> s := !s ^ d ^ ",") l
		in
		String.sub !s 0 ((String.length !s) -2)
	end

let to_string l =
	let s = ref ""
	in let _ = List.iter (fun d -> s:= !s^d)
	in !s

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
	| CreateObject(s, el) 	-> "new " ^ s ^ "(" ^ addComma (List.map str_of_expr el) ^ ")"
	| ObjAccess(e1, e2)	-> "(" ^ str_of_expr e1 ^ ").(" ^ str_of_expr e2 ^")"
	| TupleCreate(dl, el)	-> "new Tuple<" ^ addComma (List.map str_of_type dl) ^ "> (" ^ addComma (List.map str_of_expr el)
	| TupleAccess(e1, e2)	-> "(" ^ str_of_expr e1 ^ ")<<" ^ str_of_expr e2 ^ ">>"
	| ArrayCreate(dt, el)	-> "new " ^ str_of_type dt ^ to_string (List.map (fun e -> "[" ^ str_of_expr e ^ "]") el) 
	| ArrayAccess(e, el)	-> "(" ^ str_of_expr e ^ ")[" ^ to_string (List.map (fun e -> "[" ^ str_of_expr e ^ "]") el)
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

