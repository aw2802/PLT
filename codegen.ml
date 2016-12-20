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

let boolean_True =  L.const_int i1_t 1;;
let boolean_False = L.const_int i1_t 0;;

let str_t = L.pointer_type i8_t;; 
let void_t = L.void_type context;; (* void *)

let global_var_table:(string, llvalue) Hashtbl.t = Hashtbl.create 100
let class_private_vars:(string, llvalue) Hashtbl.t = Hashtbl.create 100 (*Must be cleared after class build*)
let local_var_table:(string, llvalue) Hashtbl.t = Hashtbl.create 100 (*Must be cleared evertime after a function is built*)
let struct_typ_table:(string, lltype) Hashtbl.t = Hashtbl.create 100
let struct_field_idx_table:(string, int) Hashtbl.t = Hashtbl.create 100 

let rec get_ptr_type datatype = match datatype with
		A.Arraytype(t, 0) -> get_llvm_type t
	|	A.Arraytype(t, 1) -> L.pointer_type (get_llvm_type t)
	|	A.Arraytype(t, i) -> L.pointer_type (get_ptr_type (A.Arraytype(t, (i-1))))
	| 	_ -> raise(Failure("InvalidStructType Array Pointer Type"))

and get_llvm_type datatype = match datatype with (* LLVM type for AST type *)
	  A.JChar -> i8_t
	| A.JVoid -> void_t
	| A.JBoolean -> i1_t
	| A.JFloat -> f_t
	| A.JInt -> i32_t
	| A.Object(s) -> L.pointer_type(find_llvm_struct_type s)
	| A.Arraytype(data_type, i) ->  get_ptr_type (A.Arraytype(data_type, (i)))
	| A.Tuple(dt_list) -> L.pointer_type(find_llvm_tuple_type dt_list)
	| _ -> raise(Failure("Invalid Data Type"))

and find_llvm_tuple_type dt_list =
	let type_list = List.map (function dt -> get_llvm_type dt) dt_list in
	let type_array = (Array.of_list type_list) in
	L.packed_struct_type context type_array

and find_llvm_struct_type name = 
	try Hashtbl.find struct_typ_table name
	with | Not_found -> raise(Failure ("undeclared struct "^ name))

let find_func_in_module fname = 
	match (L.lookup_function fname the_module) with
		  None -> raise(Failure("Function: " ^ fname ^ " not found in module."))
		| Some f -> f

(** code gen top level begins here **)

let translate sast = 
	let classes = sast.classes in
	let main = sast.main in 
	let functions = sast.functions in
	
	let util_func () = 
		let printf_t = L.var_arg_function_type i32_t [| pointer_type i8_t |] in
		let malloc_t = L.function_type (str_t) [| i32_t |] in
		let lookup_t = L.function_type (pointer_type i64_t) [| i32_t; i32_t |] in

		let _ = L.declare_function "printf" printf_t the_module in
		let _ = L.declare_function "malloc" malloc_t the_module in
		let _ = L.define_function "lookup" lookup_t the_module in
		()
	in
	let _ = util_func () in
	
	let zero = const_int i32_t 0 in

	(*Define Classes*)
	
	let add_classes_to_hashTable c =
		let struct_typ = L.named_struct_type context c.scname in
		Hashtbl.add struct_typ_table c.scname struct_typ
	in
	let _ = List.map add_classes_to_hashTable classes in

	let define_vardecl c =
		List.iteri (
			fun i v ->
	        	Hashtbl.add global_var_table v.svname boolean_True;
	    	) 
	    c.scbody.svariables; 
	in
	let _ = List.map define_vardecl classes in

	let define_classes c = 
		let struct_t = Hashtbl.find struct_typ_table c.scname in
		let type_list = List.map (function sv -> get_llvm_type sv.svtype) c.scbody.svariables in
		let name_list = List.map (function sv -> sv.svname) c.scbody.svariables in
		let type_list = i32_t :: type_list in
		let name_list = ".key" :: name_list in
		let type_array = (Array.of_list type_list) in
		List.iteri (
			fun i f ->
	        let n = c.scname ^ "." ^ f in
	        Hashtbl.add struct_field_idx_table n i;
	    	) 
	    name_list; 
		L.struct_set_body struct_t type_array true
	in
	let _ = List.map define_classes classes in	

	(*Define Functions*)
	let define_functions f =
		let fname = f.sfname in
		let is_var_arg = ref false in
		let types_of_parameters = List.rev ( List.fold_left 
		(fun l -> 
			(function 
			  sformal-> get_llvm_type sformal.sformal_type::l 
			| _ -> ignore(is_var_arg = ref true); l
			)
		)
		[]  f.sfformals)	
		in 

		let fty = 
			if !is_var_arg 
				then L.var_arg_function_type (get_llvm_type f.sfreturn)
				(Array.of_list types_of_parameters)
			else L.function_type (get_llvm_type f.sfreturn)
				(Array.of_list types_of_parameters)			
		in 
		L.define_function fname fty the_module (*The function name should be Class.fname?*)
	in
	let _ =  List.map define_functions functions in

	let define_constructors c =
		List.map define_functions c.scbody.sconstructors
	in
	let _ = List.map define_constructors classes in


	(*Stmt and expr handling*)

	let rec stmt_gen llbuilder = function 
		  SBlock sl        ->	generate_block sl llbuilder
 		| SExpr (se, _)    ->   expr_gen llbuilder se
		| SVarDecl sv           ->  generate_vardecl sv.svscope sv.svtype sv.svname sv.svexpr llbuilder
		| SLocalVarDecl (dt, vname, vexpr)		-> generate_local_vardecl dt vname vexpr llbuilder
		| SIf(e, s1, s2) -> generate_if e s1 s2 llbuilder
		| SWhile(e, s) -> generate_while e s llbuilder
		| SFor(e1, e2, e3, s) -> generate_for e1 e2 e3 s llbuilder
		| SReturn(e, d)		-> generate_return e d llbuilder
	
	and generate_block sl llbuilder = 
		try List.hd (List.map (stmt_gen llbuilder) sl) with 
		| Failure("hd") -> raise(Failure("No body"));

	and generate_return e d llbuilder =
		match e with
		| SNoexpr -> L.build_ret_void llbuilder
		| _ -> L.build_ret (expr_gen llbuilder e) llbuilder

	and generate_vardecl scope datatype vname expr llbuilder =
		let allocatedMemory = L.build_alloca (get_llvm_type datatype) vname llbuilder in
		Hashtbl.add 
			(match scope with
			| A.Public -> global_var_table 
			| A.Private -> class_private_vars) vname allocatedMemory;

		let variable_value = expr_gen llbuilder expr in 
			match expr with
			| SNoexpr -> allocatedMemory
			| _ -> L.build_store variable_value allocatedMemory llbuilder; variable_value

	and generate_local_vardecl datatype vname expr llbuilder =
		let allocatedMemory = L.build_alloca (get_llvm_type datatype) vname llbuilder in
		Hashtbl.add local_var_table vname allocatedMemory;
		let variable_value = expr_gen llbuilder expr in 
			match expr with
			| SNoexpr -> allocatedMemory
			| _ -> ignore (L.build_store variable_value allocatedMemory llbuilder); variable_value
			
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

		whileStatement

	and generate_for e1 e2 e3 s llbuilder =
		expr_gen llbuilder e1;
		let whileBody = SBlock [s; SExpr(e3, JInt)] in  (* JInt hardcoded*)
		generate_while e2 whileBody llbuilder

	and generate_if e s1 s2 llbuilder =
		let boolean_condition = 
		match e with
			| SId (n, dt)	-> get_value true n llbuilder 
			| _ -> expr_gen llbuilder e 
		in

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

		ifStatement

	and expr_gen llbuilder = function
		  SInt_Lit (i)     ->	L.const_int i32_t i
		| SBoolean_Lit (b) ->	if b then L.const_int i1_t 1 else L.const_int i1_t 0
		| SFloat_Lit (f)   ->	L.const_float f_t  f
		| SChar_Lit (c)    ->	L.const_int i8_t (Char.code c)
		| SString_Lit (s)  ->	build_global_stringptr s "tmp" llbuilder
		(*SNull*)
		| SId (n, dt)		-> get_value false n llbuilder 
		| SBinop(e1, op, e2, dt) -> binop_gen e1 op e2 llbuilder
		| SUnop(op, e, dt)      -> unop_gen op e llbuilder
		| SAssign (e1, e2, dt)	-> assign_to_variable e1 e2 llbuilder
		| SCreateObject(id, el, d) -> generate_object_create id el llbuilder
		| SFuncCall (fname, expr_list, d, _) -> generate_function_call fname expr_list d llbuilder
		| SNoexpr -> L.build_add (L.const_int i32_t 0) (L.const_int i32_t 0) "nop" llbuilder
		| SArrayCreate (datatype, el, d)	-> generate_array datatype el llbuilder
		| SArrayAccess(e, el, d) -> generate_array_access true e el llbuilder
		| STupleCreate(dt_list, el, d) -> generate_create_tuples dt_list el llbuilder
		| STupleAccess(e1, e2, d) -> generate_tuple_access false e1 e2 llbuilder 
		| _ -> raise(Failure("No match for expression"))

	and generate_create_tuples dt_list expr_list llbuilder =
		let type_list = List.map (function dt -> get_llvm_type dt) dt_list in
		(*let type_list = i32_t :: type_list in *)
		let type_array = (Array.of_list type_list) in
		let struct_type = L.packed_struct_type context type_array in
		let vname = "dummy" in
		let allocatedMemory = L.build_alloca struct_type vname llbuilder in
		List.iteri (
			fun i f ->
	        let tuple_value = L.build_struct_gep allocatedMemory i "temp" llbuilder in
	        	ignore(L.build_store (match f with 			
	        			| SId(id, d) -> get_value true id llbuilder
						| SArrayAccess(e, el, d) -> generate_array_access true e el llbuilder
						| STupleAccess(e1, e2, d) -> generate_tuple_access true e1 e2 llbuilder 
						| _ -> expr_gen llbuilder f) tuple_value llbuilder);
	    ) expr_list; 
	     
		L.build_pointercast allocatedMemory (L.pointer_type struct_type) "tupleMemAlloc" llbuilder

	and generate_tuple_access deref e1 e2 llbuilder =
		let vname = "dummy" in
		let index =  match e2 with
				| SInt_Lit(i) -> i
				| _ -> raise(Failure("Not an int"))
			in
		let tuple = match e1 with
			| SId(id, d) -> get_value true id llbuilder 
			| _ -> raise(Failure("Not an id"))
		in
		let tuple_value = L.build_struct_gep tuple index vname llbuilder in
		if deref 
			then L.build_load tuple_value vname llbuilder
			else tuple_value

	and generate_array_access deref e el llbuilder =
		match el with
		| [h] -> let index = match h with
					| SId(id, d) -> get_value true id llbuilder 
					| _ -> expr_gen llbuilder h 
				in
				let index = L.build_add index (const_int i32_t 1) "1tmp" llbuilder in
    			let arr = match e with
    				| SId(n, dt) -> get_value true n llbuilder 
    				| _ -> raise(Failure("Can't access array!"))
    			in
    			let _val = L.build_gep arr [| index |] "2tmp" llbuilder in
    			if deref
    				then build_load _val "3tmp" llbuilder 
    				else _val
		| _ ->  raise(Failure("Two dimentional array not supported"))

	and generate_array datatype expr_list llbuilder =
		match expr_list with
		| [h] -> generate_one_d_array datatype (expr_gen llbuilder h) llbuilder
		| _ ->  raise(Failure("Two dimentional array not supported"))
		(*| [h;s] -> generate_one_d_array datatype (expr_gen llbuilder h) (expr_gen llbuilder s) llbuilder *)
	
	and generate_one_d_array datatype size llbuilder =
		let t = get_llvm_type datatype in

		let size_t = L.build_intcast (L.size_of t) i32_t "4tmp" llbuilder in
		let size = L.build_mul size_t size "5tmp" llbuilder in
		let size_real = L.build_add size (L.const_int i32_t 1) "arr_size" llbuilder in
		
	    let arr = L.build_array_malloc t size_real "6tmp" llbuilder in
		let arr = L.build_pointercast arr (pointer_type t) "7tmp" llbuilder in

		let arr_len_ptr = L.build_pointercast arr (pointer_type i32_t) "8tmp" llbuilder in

		ignore(L.build_store size_real arr_len_ptr llbuilder); 
		initialise_array arr_len_ptr size_real (const_int i32_t 0) 0 llbuilder;
		arr

	and initialise_array arr arr_len init_val start_pos llbuilder =
		let new_block label =
			let f = L.block_parent (L.insertion_block llbuilder) in
			L.append_block (context) label f
		in
  		let bbcurr = L.insertion_block llbuilder in
  		let bbcond = new_block "array.cond" in
  		let bbbody = new_block "array.init" in
  		let bbdone = new_block "array.done" in
  		ignore (L.build_br bbcond llbuilder);
  		L.position_at_end bbcond llbuilder;

	  	(* Counter into the length of the array *)
	  	let counter = L.build_phi [const_int i32_t start_pos, bbcurr] "counter" llbuilder in
	  	add_incoming ((build_add counter (const_int i32_t 1) "tmp" llbuilder), bbbody) counter;
	  	let cmp = build_icmp Icmp.Slt counter arr_len "tmp" llbuilder in
	  	ignore (build_cond_br cmp bbbody bbdone llbuilder);
	  	position_at_end bbbody llbuilder;

	  	(* Assign array position to init_val *)
	  	let arr_ptr = build_gep arr [| counter |] "tmp" llbuilder in
	  	ignore (build_store init_val arr_ptr llbuilder);
	  	ignore (build_br bbcond llbuilder);
	  	position_at_end bbdone llbuilder

	and generate_function_call fname expr_list d llbuilder =
		match fname with
			| "print" -> print_func_gen "" expr_list llbuilder
			| "println" -> print_func_gen "\n" expr_list llbuilder
			| _ -> 	let f = find_func_in_module fname in
					let map_param_to_llvalue llbuilder e = match e with
						| SId(id, d) -> get_value true id llbuilder
						| SArrayAccess(e, el, d) -> generate_array_access true e el llbuilder
						| STupleAccess(e1, e2, d) -> generate_tuple_access true e1 e2 llbuilder 
						| _ -> expr_gen llbuilder e
					in
					let params = List.map (map_param_to_llvalue llbuilder) expr_list in (*Fix passing variable to function*)
					L.build_call f (Array.of_list params) (fname^"_result") llbuilder

	and generate_object_create id el llbuilder =
		let f = find_func_in_module id in
		let params = List.map (expr_gen llbuilder) el in
		let obj = L.build_call f (Array.of_list params) "tmp" llbuilder in
		obj 

	and binop_gen e1 op e2 llbuilder = 
		let value1 =  match e1 with
			| SId(id, d) -> get_value true id llbuilder
			| SArrayAccess(e, el, d) -> generate_array_access true e el llbuilder
			| STupleAccess(e1, e2, d) -> generate_tuple_access true e1 e2 llbuilder 
			| _ -> expr_gen llbuilder e1
		in
		let value2 = match e2 with
			| SId(id, d) -> get_value true id llbuilder
			| SArrayAccess(e, el, d) -> generate_array_access true e el llbuilder
			| STupleAccess(e1, e2, d) -> generate_tuple_access true e1 e2 llbuilder 
			| _ -> expr_gen llbuilder e2	
		in

		let int_binop value1 value2 llbuilder = (match op with
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
		) value1 value2 "binop" llbuilder
		in

		let float_binop value1 value2 llbuilder = (match op with
			  Add 		-> L.build_fadd 
			| Sub 		-> L.build_fsub
			| Mult 		-> L.build_fmul 
			| Div 		-> L.build_fdiv 
			| Equal 	-> L.build_fcmp L.Fcmp.Oeq 
			| Neq 		-> L.build_fcmp L.Fcmp.One 
			| Less 		-> L.build_fcmp L.Fcmp.Ult 
			| Leq 		-> L.build_fcmp L.Fcmp.Ole 
			| Greater	-> L.build_fcmp L.Fcmp.Ogt 
			| Geq 		-> L.build_fcmp L.Fcmp.Oge
			| And		-> L.build_and
			| Or 		-> L.build_or 
			| _ 		-> raise(Failure("Invalid operator for ints"))
		) value1 value2 "binop" llbuilder

		in

		let decide_on_type e1 e2 llbuilder =
			match ((Semant.typOFSexpr e1), (Semant.typOFSexpr e2)) with
			| (JInt,JInt) -> int_binop value1 value2 llbuilder
			| (JInt, JFloat) -> float_binop value1 value2 llbuilder
			| (JFloat, JInt) -> float_binop value1 value2 llbuilder
			| (JInt, _) -> int_binop value1 value2 llbuilder
			| (_, JInt) -> int_binop value1 value2 llbuilder
			| _ -> raise(Failure("Invalid datatype for binop"))

		in

		decide_on_type e1 e2 llbuilder


	and unop_gen op e llbuilder = 
		let exp_type = Semant.typOFSexpr e in
		let value = expr_gen llbuilder e in
		(match (op, exp_type) with 
			  (Not, JBoolean) -> L.build_not 
			| (Sub, JInt) -> L.build_neg 
			| (Sub, JFloat) -> L.build_fneg
			| _ -> raise(Failure("Invalid unop usage"))
		) value "tmp" llbuilder

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

	and assign_to_variable e1 e2 llbuilder =
		let vmemory = match e1 with
			| SId(s, d) -> get_value false s llbuilder
			| SArrayAccess(e, el, d) -> generate_array_access false e el llbuilder
			| STupleAccess(e1, e2, d) -> generate_tuple_access false e1 e2 llbuilder 
		in
		let value = match e2 with
		| SId(id, d) -> get_value true id llbuilder
		| _ -> expr_gen llbuilder e2
		in
		L.build_store value vmemory llbuilder

	and print_func_gen newLine expr_list llbuilder =
		let printf = find_func_in_module "printf" in
		let tmp_count = ref 0 in
		let incr_tmp = fun x -> incr tmp_count in
		let map_expr_to_printfexpr expr = match expr with
			| SId(id, d) -> (match d with
							| A.JBoolean -> incr_tmp ();
											let tmp_var = "print_bool" in
											let trueStr = SString_Lit("true") in
											let falseStr = SString_Lit("false") in
											let id = SId(tmp_var, Arraytype(JChar, 1)) in 
											ignore(stmt_gen llbuilder (SLocalVarDecl(Arraytype(JChar, 1), tmp_var, SNoexpr)));
											ignore(stmt_gen llbuilder (SIf(expr, 
											SExpr(SAssign(id, trueStr, Arraytype(JChar, 1)), Arraytype(JChar, 1)), 
											SExpr(SAssign(id, falseStr, Arraytype(JChar, 1)), Arraytype(JChar, 1)))));
											expr_gen llbuilder id
							| _ -> get_value true id llbuilder)
			| STupleAccess(e1, e2, d) -> generate_tuple_access true e1 e2 llbuilder 
			| SBoolean_Lit (b) ->	if b then (expr_gen llbuilder (SString_Lit("true"))) else (expr_gen llbuilder (SString_Lit("false")))
			| _ -> expr_gen llbuilder expr
		in
		let params = List.map map_expr_to_printfexpr expr_list in
		let expr_types = List.map (Semant.typOFSexpr) expr_list in

		let map_expr_to_type e = match e with
			JInt     ->	"%d"
		| JBoolean	 ->	"%s" (*needs to be implemented*)
		| JFloat	 ->	"%f"
		| JChar		 ->	"%c"
		| Arraytype(JChar, 1) 	-> "%s"
		| _ 		-> raise (Failure("Print invalid type"))

		in
		let print_types = List.fold_left (fun s t -> s ^ map_expr_to_type t) "" expr_types in
		let s = build_global_stringptr (print_types ^ newLine) "printf" llbuilder in

  		(**	let zero = const_int i32_t 0 in**)
  		let s = build_in_bounds_gep s [| zero |] "printf" llbuilder in

  		L.build_call printf (Array.of_list (s :: params)) "printf" llbuilder
	in

	(*Function generation*)

	let build_function sfunc_decl =
		Hashtbl.clear local_var_table;

		let f = find_func_in_module sfunc_decl.sfname in 	
		let llbuilder = L.builder_at_end context (L.entry_block f) in 

		(*L.position_at_end (L.entry_block f) llbuilder; *)

		let init_formals f sfformals =
			let sfformals = Array.of_list (sfformals) in
			Array.iteri (
				fun i a ->
		        	let formal = sfformals.(i) in
		        	let allocatedMemory = stmt_gen llbuilder (SLocalVarDecl(formal.sformal_type, formal.sformal_name, SNoexpr)) in
					let n = formal.sformal_name in
		        	set_value_name n a;
		        	ignore (L.build_store a allocatedMemory llbuilder);
		    ) 
		    (params f)
		in
		let _ = init_formals f sfunc_decl.sfformals in 
		let _ = stmt_gen llbuilder (SBlock (sfunc_decl.sfbody)) in 
		if sfunc_decl.sfreturn = JVoid
		then ignore (L.build_ret_void llbuilder);
		()
	in
	let _ = List.map build_function functions in

	let build_constructors class_name =

		(*If a class has multiple constructors it will get overwritten at the moment*)
		let build_constructor constructor = 
			Hashtbl.clear local_var_table;
		
			let f = find_func_in_module class_name.scname in 	
			let llbuilder = L.builder_at_end context (L.entry_block f) in

			let struct_type = find_llvm_struct_type class_name.scname in
			let allocatedMemory = L.build_alloca struct_type "object" llbuilder in    
			List.iteri (
				fun i f ->
				let expr = f.svexpr in
	        	let tuple_value = L.build_struct_gep allocatedMemory i "temp" llbuilder in
	        	ignore(L.build_store (match expr with 			
	        		| SId(id, d) -> get_value true id llbuilder
					| SArrayAccess(e, el, d) -> generate_array_access true e el llbuilder
					| STupleAccess(e1, e2, d) -> generate_tuple_access true e1 e2 llbuilder 
					| _ -> expr_gen llbuilder expr) tuple_value llbuilder);
	        	
	        	let scope = f.svscope in
	        	Hashtbl.add (match scope with
						| A.Public -> global_var_table 
						| A.Private -> class_private_vars) f.svname tuple_value;
	    	) class_name.scbody.svariables; 

			let pointer_to_class = L.build_pointercast allocatedMemory (L.pointer_type struct_type) "tupleMemAlloc" llbuilder in

			let init_formals f sfformals =
				let sfformals = Array.of_list (sfformals) in
				Array.iteri (
					fun i a ->
			        	let formal = sfformals.(i) in
		        		let varMem = stmt_gen llbuilder (SLocalVarDecl(formal.sformal_type, formal.sformal_name, SNoexpr)) in
		        		let n = formal.sformal_name in
		        		set_value_name n a;
		        		ignore (L.build_store a varMem llbuilder);
			    ) 
			    (params f)
			in
			let _ = init_formals f constructor.sfformals in 
			let _ = stmt_gen llbuilder (SBlock (constructor.sfbody)) in 

			L.build_ret pointer_to_class llbuilder
		in 
		List.map build_constructor class_name.scbody.sconstructors in
	let _ =  List.map build_constructors classes in 


	(*Main method generation*)
	let build_main main =
		let fty = L.function_type i32_t[||] in 
		let f = L.define_function "main" fty the_module in 	
		let llbuilder = L.builder_at_end context (L.entry_block f) in
			
		let _ = stmt_gen llbuilder (SBlock (main.sfbody)) in  

		L.build_ret (L.const_int i32_t 0) llbuilder
	in
	let _ = build_main main in

	(*Class generation *)

	let build_classes sclass_decl =
		let rt = L.pointer_type i64_t in
		let void_pt = L.pointer_type i64_t in
		let void_ppt = L.pointer_type void_pt in

		let f = find_func_in_module "lookup" in
		let llbuilder = L.builder_at_end context (entry_block f) in

		let len = List.length sclass_decl in
		
		let total_len = ref 0 in

		let scdecl_llvm_arr = L.build_array_alloca void_ppt (const_int i32_t len) "tmp" llbuilder in
		
		let handle_scdecl scdecl = 
			let index = try Hashtbl.find Semant.classIndices scdecl.scname with 
			| Not_found -> raise (Failure("can't find classname" ^ scdecl.scname)) in
			let len = List.length scdecl.scbody.smethods in
			let sfdecl_llvm_arr = L.build_array_alloca void_pt (const_int i32_t len) "tmp" llbuilder in

			let handle_fdecl i sfdecl = 
				let fptr = find_func_in_module sfdecl.sfname in
				let fptr = L.build_pointercast fptr void_pt "tmp" llbuilder in

				let ep = L.build_gep sfdecl_llvm_arr [| (const_int i32_t i) |] "tmp" llbuilder in
				ignore(L.build_store fptr ep llbuilder);
			in 
			List.iteri handle_fdecl scdecl.scbody.smethods;
			total_len := !total_len + len;

			let ep = L.build_gep scdecl_llvm_arr [| (const_int i32_t index) |] "tmp" llbuilder in
			ignore(build_store sfdecl_llvm_arr ep llbuilder);
		in
		List.iter handle_scdecl sclass_decl;

		let c_index = param f 0 in (*breaks*)
		let f_index = param f 1 in
		L.set_value_name "c_index" c_index;
		L.set_value_name "f_index" f_index;

		if !total_len == 0 then
			L.build_ret (const_null rt) llbuilder
		else
			let vtbl = L.build_gep scdecl_llvm_arr [| c_index |] "tmp" llbuilder in
			let vtbl = L.build_load vtbl "tmp" llbuilder in
			let fptr = L.build_gep vtbl [| f_index |] "tmp" llbuilder in
			let fptr = L.build_load fptr "tmp" llbuilder in

			L.build_ret fptr llbuilder 
	in
	let _ = build_classes classes in

	the_module;

