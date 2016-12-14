open Utils
open Llvm
open Hashtbl
open Ast
open Sast
open Semant

module L = Llvm
module A = Ast

let context = global_context ()
let the_module = create_module context "javapm"
let builder = builder context

let i32_t = L.i32_type context;; (* integer *)
let i8_t = L.i8_type context;; (* printf format string *)
let i1_t = L.i1_type context;; (* boolean *)
let i64_t = L.i64_type context ;; (* idk *)
let f_t = L.double_type context;; (* float *)

let str_t = L.pointer_type i8_t;; 
let void_t = L.void_type context;; (* void *)

let global_var_table:(string, llvalue) Hashtbl.t = Hashtbl.create 100
let local_var_table:(string, llvalue) Hashtbl.t = Hashtbl.create 100 (*Must be cleared evertime after a function is built*)

let rec get_llvm_type datatype = match datatype with (* LLVM type for AST type *)
	  A.JChar -> i8_t
	| A.JVoid -> void_t
	| A.JBoolean -> i1_t
	| A.JFloat -> f_t
	| A.JInt -> i32_t
	| _ -> raise(Failure("Invalid Data Type"))
	(** | A.Object --> @TODO **) 

let find_func_in_module fname = 
	match (L.lookup_function fname the_module) with
		  None -> raise(Failure("Function: " ^ fname ^ " not found in module."))
		| Some f -> f

(** code gen top level begins here **)

let translate sast = 
	let classes = sast.classes in
	let main = sast.main in 
	
	let util_func () = 
		let printf_t = L.var_arg_function_type i32_t [| pointer_type i8_t |] in
		let malloc_t = L.function_type (str_t) [| i32_t |] in
		let lookup_t = L.function_type (pointer_type i64_t) [| i32_t; i32_t |] in

		let _ = L.declare_function "printf" printf_t the_module in
		let _ = L.declare_function "malloc" malloc_t the_module in
		let _ = L.declare_function "lookup" lookup_t the_module in
		()
	in
	let _ = util_func () in
	
	let zero = const_int i32_t 0 in
	let rec stmt_gen llbuilder = function 
		  SBlock sl        ->	List.hd (List.map (stmt_gen llbuilder) sl)
		| SExpr (se, _)	   ->	expr_gen llbuilder se
		| SVarDecl sv		->	
			let vardecl_gen datatype vname expr llbuilder =
				let allocatedMemory = L.build_alloca (get_llvm_type datatype) vname llbuilder in
					Hashtbl.add global_var_table vname allocatedMemory;
				let variable_value = expr_gen llbuilder expr in 
					match expr with
					| SNoexpr -> allocatedMemory
					| _ -> ignore (L.build_store variable_value allocatedMemory llbuilder); variable_value
			in
			vardecl_gen sv.svtype sv.svname sv.svexpr llbuilder
		| SLocalVarDecl (dt, vname, vexpr)		->
			let local_vardecl_gen datatype vname expr llbuilder =
				let allocatedMemory = L.build_alloca (get_llvm_type datatype) vname llbuilder in
					Hashtbl.add local_var_table vname allocatedMemory;
				let variable_value = expr_gen llbuilder expr in 
					match expr with
					| SNoexpr -> allocatedMemory
					| _ -> ignore (L.build_store variable_value allocatedMemory llbuilder); variable_value
			in
			local_vardecl_gen dt vname vexpr llbuilder

	and expr_gen llbuilder = function
		  SInt_Lit (i)     ->	L.const_int i32_t i
		| SBoolean_Lit (b) ->	if b then L.const_int i1_t 1 else L.const_int i1_t 0
		| SFloat_Lit (f)   ->	L.const_float f_t  f
		| SChar_Lit (c)    ->	L.const_int i8_t (Char.code c)
		| SString_Lit (s)  ->	build_global_stringptr s "tmp" llbuilder
		(*SNull*)
		| SId (n, dt)		-> get_value false n llbuilder (*Dn't know if it is returning an OCaml variable with the value or if it is returning a value*)
		(*SBinop*)
		| SAssign (e1, e2, dt)	-> assign_to_variable (expr_gen llbuilder e1) e2 llbuilder
		| SFuncCall (fname, expr_list, d, _) -> (*Need to call a regular fuction too*)
			let reserved_func_gen llbuilder d expr_list = function
			  "print" -> print_func_gen expr_list llbuilder
			  | _ as call_name -> raise(Failure("function call not found: "^ call_name))
			in
			reserved_func_gen llbuilder d expr_list fname

	and get_value deref vname llbuilder = 
		if deref then
		let var = try Hashtbl.find global_var_table vname with 
		| Not_found -> try Hashtbl.find local_var_table vname with 
			| Not_found -> raise (Failure("unknown variable name " ^ vname))
		in
		L.build_load var vname llbuilder
		
	else
		let var = try Hashtbl.find global_var_table vname with 
		| Not_found -> try Hashtbl.find local_var_table vname with 
			| Not_found -> raise (Failure("unknown variable name " ^ vname))
		in
		var

	and assign_to_variable vmemory e2 llbuilder =
		let value = match e2 with
		| SId(id, d) -> get_value true id llbuilder
		| _ -> expr_gen llbuilder e2
		in
		L.build_store value vmemory llbuilder

	and print_func_gen expr_list llbuilder =
		let printf = find_func_in_module "printf" in
		let map_expr_to_printfexpr expr = expr_gen llbuilder expr in
		let params = List.map map_expr_to_printfexpr expr_list in
		let expr_types = List.map (Semant.typOFSexpr) expr_list in

		let map_expr_to_type e = match e with
			JInt     ->	"%d"
		| JBoolean	 ->	"%s" (*needs to be implemented*)
		| JFloat	 ->	"%f"
		| JChar		 ->	"%c"
		| JString	 -> "%s"
		| _ 			-> raise (Failure("Print invalid type"))

		in
		let print_types = List.fold_left (fun s t -> s ^ map_expr_to_type t) "" expr_types in
		let s = build_global_stringptr (print_types ^ "\n") "printf" llbuilder in

  		(**	let zero = const_int i32_t 0 in**)
  		let s = build_in_bounds_gep s [| zero |] "printf" llbuilder in

  		L.build_call printf (Array.of_list (s :: params)) "printf" llbuilder
	in

	(*Function generation*)


	(*Main method generation*)
	let build_main main =
		    let fty = L.function_type i32_t[||] in 
			let f = L.define_function "main" fty the_module in 	
			let llbuilder = L.builder_at_end context (L.entry_block f) in
			
			let _ = stmt_gen llbuilder (SBlock (main.sfbody)) in  

			L.build_ret (L.const_int i32_t 0) llbuilder
		in
		let _ = build_main main in

	(*Class generation*)
(*
	let build_classes sclass_decl =
		let rt = L.pointer_type i64_t in
		let void_pt = L.pointer_type i64_t in
		let void_ppt = L.pointer_type void_pt in

		let f = find_func_in_module "lookup" in
		let llbuilder = L.builder_at_end context (entry_block f) in

	let _ = List.map build_classes classes in	
*)
	the_module;

