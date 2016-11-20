(* 
LLVM Code Generator
*)

open Utils
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
let class_variable_idx_table:(string, int) Hashtbl.t = Hashtbl.create 100

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
	
	(* for each class in the program, add classname --> class_type mapping *)
	let add_class_type_table cls = 
		let class_type = L.named_struct_type context cls.scname in
		Hashtbl.add class_type_table cls.scname class_type
	in
	
	let _ = List.map add_class_type_table classes in 
	
	let generate_class_struct c = 
		let class_t = Hashtbl.find class_type_table c.scname in
		let scope_list = List.map (function svdecl -> str_of_scope svdecl.svscope) c.scbody.svariables in 
		let type_list = List.map (function svdecl -> get_llvm_type svdecl.svtype) c.scbody.svariables in 
		let name_list = List.map (function svdecl -> svdecl.svname) c.scbody.svariables in
		let scope_list = "scope" :: scope_list in
		let type_list = i32_t :: type_list in
		let name_list = ".key"  :: name_list in
		let type_array = (Array.of_list type_list) in
		List.iteri (
			fun i f ->
			let n = c.scname ^ "." ^ f ^ "_" ^ List.nth scope_list i in
			Hashtbl.add class_variable_idx_table n i;
			)
		name_list;
		(* @TODO: scope_list --> an array for const access + figure out how to deal
		with scoping issues re: storing a var's scope *)

		L.struct_set_body class_t type_array true
	in
	let _ = List.map generate_class_struct classes in
	
	(** function definition begins here **)
	let func_define sfdecl = 
		
		let fscope = sfdecl.sfscope in
		let fname = sfdecl.sfname in
		let is_actual_param = ref false in
		let params = List.map (fun f -> get_llvm_type f.sformal_type) sfdecl.sfformals in
		let fty = L.function_type (get_llvm_type sfdecl.sfreturn) (Array.of_list params)
		in
		L.define_function fname fty the_module
	in
	let _ = List.map func_define functions in
	
	(** function generation utils begins here **)
	let rec stmt_gen llbuilder = function
	  SBlock sl	 -> List.hd (List.map (stmt_gen llbuilder) sl)
	(*@TODO	| SVarDecl vdecl -> *)
	| SReturn (e,d)	 -> 
		let return_gen d expr llbuilder = 
			match expr with
				(* @TODO SId(name, d) -> *)
				(* @TODO Object *)
				 SNoexpr -> build_ret_void llbuilder
				| _ 	  -> build_ret (expr_gen llbuilder expr) llbuilder
			in
			return_gen d e builder
	| SExpr (se, _)  -> expr_gen llbuilder se
	
	(* control flow *)
	| SIf (e, s1, s2)->
		let if_gen expr then_stmt else_stmt llbuilder = 
			let condition = expr_gen llbuilder expr in
			let start_block = L.insertion_block llbuilder in
			let parent_function = L.block_parent start_block in
			let then_block   = L.append_block context "then" parent_function in
			L.position_at_end then_block llbuilder;

			let then_val = stmt_gen llbuilder then_stmt in
			let new_then_block = L.insertion_block llbuilder in
			let else_block = L.append_block context "else" parent_function in
			L.position_at_end else_block llbuilder;

			let else_val = stmt_gen llbuilder else_stmt in
			let new_else_block = L.insertion_block llbuilder in
			let merge_block = L.append_block context "ifcont" parent_function in
			L.position_at_end merge_block builder;

			let incoming = [(then_val, new_then_block); (else_val, new_else_block)] in
			let phi = L.build_phi incoming "iftmp" builder in
			L.position_at_end start_block llbuilder;
			ignore (build_cond_br condition then_block else_block llbuilder);
			position_at_end new_then_block llbuilder;
			ignore (build_br merge_block llbuilder);
			position_at_end new_else_block llbuilder;
			ignore (build_br merge_block llbuilder);
			(* set the builder to the end of the merge *)
			L.position_at_end merge_block llbuilder;
			phi
		in
		if_gen e s1 s2 llbuilder
	| SFor (se1, se2, se3, s) -> for_gen se1 se2 se3 s llbuilder
	| SWhile (se, s)	  -> while_gen se s llbuilder

	(* expression generation *)
	and expr_gen llbuilder = function
		  SInt_Lit (i)		-> L.con_int i32_t i
		| SBoolean_Lit (b)	-> if b then L.const_int i1_t 1 else L.const_int i1_t 0
		| SFloat_Lit (f)	-> L.const_int f_t (Char.code c)
		| SString_Lit (s)	-> L.build_global_stringptr s "tmp" llbuilder
		| SChar_Lit (c)		-> L.const_int i8_t (Char.code c)
		| SNull			-> L.const_null (**@TODO double check this**) 
		| SId (id, d)		-> id_gen true false is d llbuilder
		
in
print_endline ("End codegen`");
