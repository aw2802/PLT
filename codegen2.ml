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
		  SBlock sl        ->	List.fold_left stmt_gen llbuilder sl
		| SExpr (se, _)	   ->	ignore(expr_gen llbuilder se); llbuilder
		| SVarDecl sv		->	
			let vardecl_gen datatype vname expr llbuilder =
				let allocatedMemory = L.build_alloca (get_llvm_type datatype) vname llbuilder in
					Hashtbl.add global_var_table vname allocatedMemory;
				let variable_value = expr_gen llbuilder expr in 
					match expr with
					| SNoexpr -> allocatedMemory
					| _ -> ignore (L.build_store variable_value allocatedMemory llbuilder); llbuilder
			in
			vardecl_gen sv.svtype sv.svname sv.svexpr llbuilder
		| SLocalVarDecl (dt, vname, vexpr)		->
			let local_vardecl_gen datatype vname expr llbuilder =
				let allocatedMemory = L.build_alloca (get_llvm_type datatype) vname llbuilder in
					Hashtbl.add local_var_table vname allocatedMemory;
				let variable_value = expr_gen llbuilder expr in 
					match expr with
					| SNoexpr -> allocatedMemory
					| _ -> ignore (L.build_store variable_value allocatedMemory llbuilder); llbuilder
			in
			local_vardecl_gen dt vname vexpr llbuilder
		| SIf(e, s1, s2) -> generate_if e s1 s2 llbuilder
		| SWhile(e, s) -> generate_while e s llbuilder
		| SFor(e1, e2, e3, s) -> generate_for e1 e2 e3 s llbuilder

	and generate_while e s llbuilder =

		let start_block = L.insertion_block llbuilder in
		let parent_function = L.block_parent start_block in

		let pred_block = L.append_block context "while" parent_function in 
		L.build_br pred_block llbuilder;

		let body_block = L.append_block context "while_body" parent_function in
		let body_builder = L.builder_at_end context body_block in	
		
		let stmt = stmt_gen body_builder s in
		L.build_br pred_block body_builder;

		let pred_builder = L.builder_at_end context pred_block in

		let boolean_condition = expr_gen pred_builder e in
		let merge_block = L.append_block context "merge" parent_function in

		let whileStatement = L.build_cond_br boolean_condition body_block merge_block pred_builder in
		
		L.position_at_end merge_block llbuilder;

		(*whileStatement *)

	and generate_for e1 e2 e3 s llbuilder =
		expr_gen llbuilder e1;
		let sbody = stmt_gen llbuilder s in
		let endBlock = [(expr_gen llbuilder e3)] in
		let sl = List.concat sbody ; endBlock in
		let whileBlock = SBlock(sl) in

		generate_while e2 whileBlock llbuilder

	and generate_if e s1 s2 llbuilder =
		let boolean_condition = expr_gen llbuilder e in

		let start_block = L.insertion_block llbuilder in
		let parent_function = L.block_parent start_block in

		let merge_block = L.append_block context "merge" parent_function in

		let then_block = L.append_block context "then" parent_function in
		L.position_at_end then_block llbuilder;
		
		let stmt1 = stmt_gen llbuilder s1 in
		L.build_br merge_block llbuilder;

		let else_block = L.append_block context "else" parent_function in
		L.position_at_end else_block llbuilder;
		let stmt2 = stmt_gen llbuilder s2 in
		L.build_br merge_block llbuilder;

		L.position_at_end start_block llbuilder;
		let ifStatement = L.build_cond_br boolean_condition then_block else_block llbuilder in
		L.position_at_end merge_block llbuilder;

		(*ifStatement*)

	and expr_gen llbuilder = function
		  SInt_Lit (i)     ->	L.const_int i32_t i
		| SBoolean_Lit (b) ->	if b then L.const_int i1_t 1 else L.const_int i1_t 0
		| SFloat_Lit (f)   ->	L.const_float f_t  f
		| SChar_Lit (c)    ->	L.const_int i8_t (Char.code c)
		| SString_Lit (s)  ->	build_global_stringptr s "tmp" llbuilder
		(*SNull*)
		| SId (n, dt)		-> get_value false n llbuilder (*Dn't know if it is returning an OCaml variable with the value or if it is returning a value*)
		| SBinop(e1, op, e2, dt) -> binop_gen e1 op e2 llbuilder
		| SAssign (id, e, dt)	-> assign_to_variable (get_value false id llbuilder) e llbuilder
		| SFuncCall (fname, expr_list, d, _) -> (*Need to call a regular fuction too*)
			let reserved_func_gen llbuilder d expr_list = function
			  "print" -> print_func_gen expr_list llbuilder
			  | _ as call_name -> raise(Failure("function call not found: "^ call_name))
			in
			reserved_func_gen llbuilder d expr_list fname
		| SNoexpr -> L.build_add (L.const_int i32_t 0) (L.const_int i32_t 0) "nop" llbuilder
		| _ -> raise(Failure("expression not match"))

	and binop_gen e1 op e2 llbuilder = 
		let value1 =  match e1 with
			| SId(id, d) -> get_value true id llbuilder
			| _ -> expr_gen llbuilder e1
		in
		let value2 = match e2 with
			| SId(id, d) -> get_value true id llbuilder
			| _ -> expr_gen llbuilder e2	
		in

		(match op with
			  Add 		-> L.build_add 
			| Sub 		-> L.build_sub
			| Mult 		-> L.build_mul 
			| Div 		-> L.build_sdiv 
			| Equal 	-> L.build_icmp L.Icmp.Eq 
			| Neq 		-> L.build_icmp L.Icmp.Ne 
			| Less 		-> L.build_icmp L.Icmp.Slt 
			| Leq 		-> L.build_icmp L.Icmp.Sle 
			| Greater	-> L.build_icmp L.Icmp.Sgt 
			| Geq 		-> L.build_icmp L.Icmp.Sge
			| And		-> L.build_and
			| Or 		-> L.build_or 
			| _ 		-> raise(Failure("Invalid operator for ints"))
		) value1 value2 "tmp" llbuilder


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

	and assign_to_variable vmemory e llbuilder =
		let value = match e with
		| SId(id, d) -> get_value true id llbuilder
		| _ -> expr_gen llbuilder e
		in
		L.build_store value vmemory llbuilder

	and print_func_gen expr_list llbuilder =
		let printf = find_func_in_module "printf" in
		let map_expr_to_printfexpr expr = match expr with
			| SId(id, d) -> get_value true id llbuilder
			| _ -> expr_gen llbuilder expr
		in
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

