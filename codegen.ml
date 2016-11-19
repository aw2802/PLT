(* 
LLVM Code Generator
*)

open Llvm
open Hashtbl
open Ast
open Sast

module L = Llvm
module A = Ast

let context = global_context ()
let the_module = create_module context "javapm"
let builder = builder context

let stubby parameter = "stubed"

let i32_t = L.i32_type context;; (* integer *)
let i8_t = L.i8_type context;; (* printf format string *)
let i1_t = L.i1_type context;; (* boolean *)
let i64_t = L.i64_type context ;; (* idk *)
let f_t = L.double_type context;; (* float *)

let str_t = L.pointer_type i8_t;; 
let void_t = L.void_type context;; (* void *)

let class_type_table:(string, lltype) Hashtbl.t = Hashtbl.create 100

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
	let functions = sast.functions in
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
	
	(** begin generating class structure here **)
	let add_class_type_table cls = 
		let class_type = L.named_struct_type context cls.scname in
		Hashtbl.add class_type_table cls.scname class_type
	in
	
	let _ = List.map add_class_type_table classes in 
	print_endline("hi");
	(**let get_vdecl_type 
	let class_struc	**)
