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

	let rec stmt_gen llbuilder = function 
		  SBlock sl        ->	List.hd (List.map (stmt_gen llbuilder) sl)
		| SExpr (se, _)	   ->	expr_gen llbuilder se

	and expr_gen llbuilder = function
		  SInt_Lit (i)     ->	L.const_int i32_t i
		| SBoolean_Lit (b) ->	if b then L.const_int i1_t 1 else L.const_int i1_t 0
		| SFloat_Lit (f)   ->	L.const_float f_t  f
		| SChar_Lit (c)    ->	L.const_int i8_t (Char.code c)
		| SString_Lit (s)  ->	L.build_global_stringptr s "tmp" llbuilder
		| SFuncCall (fname, expr_list, d, _) -> 
			let reserved_func_gen llbuilder d expr_list = function
			  "print" -> print_func_gen expr_list llbuilder
			  | _ as call_name -> raise(Failure("function call not found: "^ call_name))
		in
		reserved_func_gen llbuilder d expr_list fname

	and print_func_gen expr_list llbuilder =
		let printf = find_func_in_module "printf" in
		let map_expr_to_printfexpr expr = expr_gen llbuilder expr in
		let params = List.map map_expr_to_printfexpr expr_list in

		let s = build_global_stringptr "%s" "printf" llbuilder in
	(**	let s = build_global_stringptr (List.hd expr_list) "printf" llbuilder in **)

  		let zero = const_int i32_t 0 in
  		let s = build_in_bounds_gep s [| zero |] "printf" llbuilder in

  		L.build_call printf (Array.of_list (s :: params)) "printf" llbuilder
	in

	let build_main main =
		    let fty = L.function_type i32_t[||] in 
			let f = L.define_function "main" fty the_module in 	
			let llbuilder = L.builder_at_end context (L.entry_block f) in
			
			let _ = stmt_gen llbuilder (SBlock (main.sfbody)) in  

			L.build_ret (L.const_int i32_t 0) llbuilder
		in
		let _ = build_main main in

	the_module;

