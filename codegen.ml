(* 
LLVM Code Generator
*)

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

let stubby parameter = "stubed"

let i32_t = L.i32_type context;; (* integer *)
let i8_t = L.i8_type context;; (* printf format string *)
let i1_t = L.i1_type context;; (* boolean *)
let i64_t = L.i64_type context ;; (* idk *)
let f_t = L.double_type context;; (* float *)

let str_t = L.pointer_type i8_t;; 
let void_t = L.void_type context;; (* void *)

let local_var_table:(string, llvalue) Hashtbl.t = Hashtbl.create 100
let formals_table:(string, llvalue) Hashtbl.t = Hashtbl.create 100

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
(**	| SFor (se1, se2, se3, s) -> for_gen se1 se2 se3 s llbuilder
	| SWhile (se, s)	  -> while_gen se s llbuilder **) 
(** comment out above lines until for_gen and while_gen are defined **)

	(* expression generation *)
	and expr_gen llbuilder = function
		  SInt_Lit (i)		-> L.const_int i32_t i
		| SBoolean_Lit (b)	-> if b then L.const_int i1_t 1 else L.const_int i1_t 0
		| SFloat_Lit (f)	-> L.const_float f_t f
		| SString_Lit (s)	-> L.build_global_stringptr s "tmp" llbuilder
		| SChar_Lit (c)		-> L.const_int i8_t (Char.code c)
	(**	| SNull			-> L.const_null @TODO double check this**) 
	(**	| SId (id, d)		-> id_gen true false id d llbuilder **)
		| SBinop (e1, op, e2, d) -> 
		     let binop_gen e1 op e2 d llbuilder =
			let t1 = Semant.typeOfSexpr e1 in
			let t2 = Semant.typeOfSexpr e2 in
			let e1 = expr_gen llbuilder e1 in
			let e2 = expr_gen llbuilder e2 in
			let float_operations op e1 e2 =	
				match op with (** examine O vs U **)
				  Add 	-> L.build_fadd e1 e2 "fadd_tmp" llbuilder
				| Sub 	-> L.build_fsub e1 e2 "fsub_tmp" llbuilder
				| Mult 	-> L.build_fmul e1 e2 "fmult_tmp" llbuilder
				| Div 	-> L.build_fdiv e1 e2 "fdiv_tmp" llbuilder
				| Equal -> L.build_fcmp e1 e2 Fcmp.Oeq e1 e2 "feq_tmp" llbuilder
				| Neq -> L.build_fcmp Fcmp.One e1 e2 "fneq_tmp" llbuilder 	
				| Less -> L.build_fcmp Fcmp.Ult e1 e2 "fless_tmp" llbuilder
				| Leq -> L.build_fcmp Fcmp.Ole e1 e2 "fleq_tmp" llbuilder
				| Greater -> L.build_fcmp Fcmp.Ogt e1 e2 "fgreat_tmp" llbuilder
				| Geq -> L.build_fcmp Fcmp.Oge e1 e2 "fgeq_tmp" llbuilder
				| _ -> raise(Failure("Invalid operator for floats."))
			in 
			let int_operations op e1 e2 = 
				match op with 
				| Add	-> L.build_add e1 e2 "add_tmp" llbuilder
				| Sub 	-> L.build_sub e1 e2 "sub_tmp" llbuilder
				| Mult 	-> L.build_mul e1 e2 "mult_tmp" llbuilder
				| Div 	-> L.build_sdiv e1 e2 "div_tmp" llbuilder
				| Equal	-> L.build_icmp Icmp.Eq e1 e2 "eq_tmp" llbuilder
				| Neq 	-> L.build_icmp Icmp.Ne	e1 e2 "neq_tmp" llbuilder
				| Less	-> L.build_icmp Icmp.Slt e1 e2 "less_tmp" llbuilder
				| Leq 	-> L.build_icmp Icmp.Sle e1 e2 "leq_tmp" llbuilder
				| Greater -> L.build_icmp Icmp.Sgt e1 e2 "great_tmp" llbuilder
				| Geq	-> L.build_icmp Icmp.Sge e1 e2 "geq_tmp" llbuilder
				| And	-> L.build_and e1 e2 "and_tmp" llbuilder
				| Or	-> L.build_or e1 e2 "or_tmp" llbuilder
				| _ 	-> raise(Failure("Invalid Operator for integer"))
			in
			let binop_type_cast lhs rhs lhsType rhsType llbuilder = 
				match (lhsType, rhsType) with
				  JInt, JInt	 -> (lhs, rhs), JInt
				| JInt, JChar	 -> (build_uitofp lhs i8_t "tmp" llbuilder, rhs), JChar
				| JInt, JFloat -> (build_sitofp lhs f_t "tmp" llbuilder, rhs), JFloat
				| JChar, JInt  -> (lhs, build_uitofp rhs i8_t "tmp" llbuilder), JChar
				| JChar, JChar -> (lhs, rhs), JChar
				| JBoolean, JBoolean -> (lhs, rhs), JBoolean
				| JFloat, JInt -> (lhs, build_sitofp rhs f_t "tmp" llbuilder), JFloat
				| JFloat, JFloat 	-> (lhs, rhs), JFloat
				| 	_ -> raise (Failure("binop type not supported"))
			in 
			let (e1, e2), d = binop_type_cast e1 e2 type1 type2 llbuilder 
			in
			
			let type_handler d = match d with (** missing object **)
				  JFloat   -> float_ops op e1 e2
				| JInt
				| JBoolean 
				| JChar    -> int_ops op e1 e2
				| _ -> raise (Failure("Invalid binop type"))
			
			in
			type_handler d
		in
		binop_gen e1 op e2 d llbuilder
	| SUnop (op, e, d) -> 
		let unop_gen op e d llbuilder = 
			let exp_t = Semant.typeOfSexpr e in
			let e = expr_gen llbuilder e in
			let unop op exp_t e = match (op, exp_t) with
			    (Not, JBoolean) -> L.build_not e "unot_tmp" llbuilder
			  | _ -> raise (Failure("unop not supported")) 
			in
			let unop_type_handler d = match d with
				  JBoolean -> unops op e_type e
				| _ -> raise(Failure("Invalid unop type "))
			in unop_type_handler d
		in unop_gen op e d llbuilder
	| SFuncCall (fname, param_list, d, _) -> 
		let reserved_func_gen llbuilder d expr_list = function
			  "print" -> print_func_gen expr_list llbuilder
			| _ as call_name -> raise(Failure("function call not found: "^ call_name))
		in
		reserved_func_gen llbuilder d expr_list fname

	and print_func_gen expr_list llbuilder =
		let printf = find_func_in_module "printf" in

		let s = build_global_stringptr "Hello, world!\n" "" llbuilder in

  		let zero = const_int i32_t 0 in
  		let s = build_in_bounds_gep s [| zero |] "" llbuilder in

  		L.build_call printf [| s |] "" builder
	in
	print_string("hi");

(*
	and print_func_gen expr_list llbuilder =
		let printf = find_func_in_module "printf" in
		let tmp_count = ref 0 in
		let incr_tmp = fun x -> incr tmp_count in
		let map_expr_to_printfexpr expr =
			let exprType = Semant.typOfSexpr expr in
			match exprType with
			JBoolean ->
				incr_tmp();
				let tmp_var = "tmp" ^ (string_of_int  !tmp_count) in
				let trueStr = SString_Lit("true") in
				let falseStr = SString_Lit("false") in
				let id = SID(tmp_var, arr_type) in
				ignore(stmt_gen llbuilder (SLocal(arr_type, tmp_var, SNoexpr)));
				ignore(
					stmt_gen llbuilder(
						SIf( expr, SExpr(SAssign(id, trueStr, arr_type), arr_type), SExpr(SAssign(id, falseStr, arr_type), arr_type)
)
					)
				);
				expr_gen llbuilder id
			| _ -> expr_gen llbuilder expr
		in
		let params = List.map map_expr_to_printfexpr expr_list in
		let param_types = List.map (Semant.typOFSexpr) expr_list in
		let map_param_to_string = function
			Arraytype(JChar, 1)	-> "%s"
			|JInt			-> "%d"
			|JFloat			-> "%f"
			|JBoolean		-> "%s"
			|JChar			-> "%c"
			|_			-> raise(Failure("Print invalid type"))
		in
		let const_str = List.fold_left(fun s t -> s ^ map_param_to_string t) "" param_types in 
		let s = expr_gen llbuilder (SString_Lit(const_str))
		let zero = const_int i32_t 0 in
		let s = L.build_in_bounds_gep s [| zero |] "tmp" llbuilder in
		L.build_call printf (Array.of_list (s :: params)) "tmp" builder


	and for_gen start cond step body llbuilder = 
		let preheader_bb = L.insertion_block llbuilder in
		let the_function = L.block_parent preheader_bb in
		let _ = expr_gen llbuilder start in
		let loop_bb = L.append_block context "loop" the_function in
		let step_bb = L.append_block context "step" the_function in 
		let cond_bb = L.append_block context "cond" the_function in

		
		let build_func sfdecl =
			Hashtbl.clear local_var_table;
			Hashtbl.clear formal_table;
			let fname = string_of_name sfdecl.sfname in **)
