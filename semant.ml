open Ast
open Sast
open Utils

module StringMap = Map.Make(String)

let classIndices: (string, int) Hashtbl.t =  Hashtbl.create 10
  
let createClassIndices cdecls=  
	let classHandler index cdecl=
	Hashtbl.add classIndices cdecl.cname index in (*scope handling is missing*)
	List.iteri classHandler cdecls

let builtinMethods = ["print"; "println"]
	
let isMain f = f.sfname = "main"

let get_methods l classy =  List.concat [classy.scbody.smethods;l]

let get_main m = try (List.hd (List.filter isMain (List.fold_left get_methods [] m))) with Failure("hd") -> raise(Failure("No main method was defined"))

let get_methods_minus_main m = snd (List.partition isMain (List.fold_left get_methods [] m))

let newEnv env = {
      	envClassName = env.envClassName;
     	envClassMaps = env.envClassMaps;
    	envClassMap  = env.envClassMap;
    	envLocals    = env.envLocals;
     	envParams    = env.envParams;
      	envReturnType = env.envReturnType;
 	envBuiltinMethods = env.envBuiltinMethods;
}

let typOFSexpr = function
		SInt_Lit(i)			-> JInt	
	| 	SBoolean_Lit(b)			-> JBoolean	
	| 	SFloat_Lit(f)			-> JFloat
	| 	SString_Lit(s) 			-> Arraytype(JChar, 1)
	| 	SChar_Lit(c) 			-> JChar
	| 	SId(_, d) 			-> d
	| 	SBinop(_, _, _, d) 		-> d
	|   SAssign(_, _, d) 		-> d
	| 	SArrayCreate(_, _, d)	-> d
	| 	SArrayAccess(_, _, d) 	-> d
	| 	SFuncCall(_, _, d, _)	-> d
	|  	SUnop(_, _, d) 			-> d 
	| 	SCreateObject(_,_,d)	-> d
	| 	SObjAccess(_,_,d) -> d
	|	STupleAccess(_,_, d) 	-> d

let convertToSast classes =
	
		let convertFormalToSast formal env =
		{
			sformal_type = formal.fvtype;
		 	sformal_name = formal.fvname;
		}
		in
		let checkBinop e1 op e2 =
			"placeholder"
		in
		let checkUnop op e env =
			"placeholder"
		in
		let checkFuncCall s el env = 
			"placeholder"
		in
		let checkAssign e1 e2 env =
			"placeholder"
(*			let typ1  = (getType e1 env)
			and typ2  = (getType e2 env)
			in
			if typ1 <> typ2
				then raise(Failure("Expected " ^ (str_of_type typ1) ^ " expression in " ^ (str_of_expr e1)))
*)		in
		let rec convertExprToSast expr env = match expr with
			  Int_Lit(i)	-> SInt_Lit(i)
			| Bool_Lit(b)	-> SBoolean_Lit(b)
			| Float_Lit(f)	-> SFloat_Lit(f)
			| String_Lit(s)	-> SString_Lit(s)
			| Char_Lit(c)	-> SChar_Lit(c)
			| Null		-> SNull
			| Noexpr	-> SNoexpr
			| Id(id)	-> SId(id, JInt (*getIdType id env*)) (** @TODO Sast has SId(string, datatype) **)
			| Binop(expr1, op, expr2)	-> (checkBinop expr1 op expr2;SBinop(convertExprToSast expr1 env, op, convertExprToSast expr2 env, JInt (*getType expr1 env)*)))
			| ArrayCreate(d, el)  -> SArrayCreate(d, (List.map (fun e -> convertExprToSast e env) el),JInt (*Arraytype(d, List.length el)*))
			| ArrayAccess(e, el)  -> SArrayAccess(convertExprToSast e env, (List.map (fun e -> convertExprToSast e env) el), JInt) (* @TODO *)
			| Assign(e1, e2)		-> (checkAssign e1 e2 env; SAssign(convertExprToSast e1 env, convertExprToSast e2 env,JInt(* getType e1 env*)))
			| FuncCall(s, el)		-> (checkFuncCall s el env; SFuncCall(s, (List.map (fun e -> convertExprToSast e env) el), JInt(*getType (FuncCall(s, el)) env*), 0))
			| Unop(op, expr)		-> (checkUnop op expr env; SUnop(op, convertExprToSast expr env, JInt (*getType expr env*)))
			| CreateObject(s,el)	-> SCreateObject(s, (List.map (fun e -> convertExprToSast e env) el), JInt (*Object(s)*))
			| ObjAccess(e1,e2) -> SObjAccess(convertExprToSast e1 env, convertExprToSast e2 env, JInt (*getType e1 env*)) (* @TODO Double check type *)
			| TupleCreate(dl, el)	-> STupleCreate(dl, (List.map (fun e -> convertExprToSast e env) el), Tuple(dl))
			| TupleAccess(e1, e2)	-> STupleAccess(convertExprToSast e1 env, convertExprToSast e2 env, JInt (*getType e1 env*)) (* @TODO Double Check type*)		
		in
		let convertVdeclToSast vdecl env = {
			svscope = vdecl.vscope;
			svtype = vdecl.vtype;
			svname = vdecl.vname;
			svexpr = convertExprToSast vdecl.vexpr env;
		} in
		let checkLocalVarDecl dt id e env =
(*			"placeholder"
			if StringMap.mem id env.envParams || StringMap.mem id env.envLocals || StringMap.mem id env.envClassMap.variableMap
				then raise(Failure("Variable name already used"))
			else if ((e <> Ast.Noexpr) && ((getType e env) <> dt))
				then raise(Failure(str_of_expr e ^ " is of type " ^ str_of_type (getType e env) ^ " but expression of type " ^ str_of_type dt ^ " was expected"));
*)			env.envLocals <- StringMap.add id dt env.envLocals 
		in
		let checkReturn e env =
			"placeholder"
(*			if getType e env <> env.envReturnType
				then raise(Failure("Return type doesn't match expected return type"))
*)		in
		let checkIf e s1 s2 env =
			"placeholder"
(*			if (getType e env) <> JBoolean
				then raise(Failure("Expected boolean expression in " ^ (str_of_expr e)))	
*)		in
		let checkFor e1 e2 e3 s env = 
			"placeholder"
(*			if (getType e2 env) <> JBoolean
				then raise(Failure("Expected boolean expression in " ^ (str_of_expr e2)))
*)		in
		let checkWhile e s env =
			"placeholder" 
(*			if (getType e env) <> JBoolean
                                then raise(Failure("Expected boolean expression in " ^ (str_of_expr e)))
*)		in	
		let rec convertStmtToSast stmt env = match stmt with
			  Block(sl)			-> SBlock(List.map (fun s -> convertStmtToSast s env) sl)
			| Expr(expr)			-> SExpr(convertExprToSast expr env, getType expr env)
(*			| VarDecl(vdecl)		-> SVarDecl(convertVdeclToSast vdecl env)
*)			| LocalVarDecl(dt, id, expr)	-> (checkLocalVarDecl dt id expr env; SLocalVarDecl(dt, id, convertExprToSast expr env))
			| Return(expr)  		-> checkReturn expr env; SReturn(convertExprToSast expr env, getType expr env)
			| If(expr, stmt1, stmt2)	-> checkIf expr stmt1 stmt2 env; SIf(convertExprToSast expr env, convertStmtToSast stmt1 (newEnv env), convertStmtToSast stmt2 (newEnv env))
			| For(expr1, expr2, expr3, stmt)-> checkFor expr1 expr2 expr3 stmt env; SFor(convertExprToSast expr1 env, convertExprToSast expr2 env, convertExprToSast expr3 env, convertStmtToSast stmt (newEnv env))
			| While(expr, stmt)		-> checkWhile expr stmt env; SWhile(convertExprToSast expr env, convertStmtToSast stmt (newEnv env))
	in
	
	(* Semantic Checking for class methods *)
	let rec strOfFormals fl = match fl with
		|  [] -> ""
		| h::t -> str_of_type h ^ (strOfFormals t)
	in
let getListOfFormalTypes c = List.map (fun f -> f.fvtype) c.fformals 
	in
	let hasDuplicateFormalNames l = 
		let result = ref false
		in let names = List.map (fun f -> f.fvname) l
		in List.iter (fun e -> result := !result || e) (List.map (fun n -> List.length (List.filter (fun s -> s = n) names) > 1) names);!result
	in
	let checkMethod func_decl classEnv =
(*		let formalTypes = getListOfFormalTypes func_decl
		in
		let checkSignature _ v =
			v.mformalTypes=formalTypes
		in
		let compareName k _ =
			k = func_decl.fname
		in
		let mp = StringMap.filter compareName classEnv.classMap.methodMap
		in
		
		let _ = StringMap.iter (fun k v -> if checkSignature k v then raise(Failure("Duplicate Method Declaration"))) mp 
		in StringMap.iter (fun kc c -> ((StringMap.iter ((fun k v -> if k=func_decl.fname && checkSignature k v then raise(Failure("Duplicate Method Declaration"))) c.methodMap));
			if StringMap.mem (strOfFormals formalTypes) c.constructorMap then raise(Failure("Duplicate Method Declaration")))) classEnv.classMaps;
		if hasDuplicateFormalNames func_decl.fformals
			then raise(Failure("Formal names must be unique"));
		let signature = {
			mscope = func_decl.fscope;
			mname = func_decl.fname;
			mformalTypes = List.map (fun fl -> fl.fvtype) func_decl.fformals;
			mReturn = func_decl.freturn;
		} in signature
	in
*)	{
		mscope = Public;
		mname = "";
		mformalTypes = [];
		mReturn = JInt
	}

	in
	let setEnvParams formals env =
			List.map (fun f -> env.envParams <- StringMap.add f.fvname f.fvtype env.envParams) formals

	in
	let convertMethodToSast func_decl classEnv =
	
		let methodSignature = checkMethod func_decl classEnv
		in
		let _ = classEnv.classMap.methodMap <- StringMap.add func_decl.fname methodSignature classEnv.classMap.methodMap
		in
		let env = {
			envClassName = classEnv.className;
			envClassMaps = classEnv.classMaps;
			envClassMap  = classEnv.classMap;
			envLocals    = StringMap.empty;
			envParams    = StringMap.empty;
			envReturnType= func_decl.freturn;
			envBuiltinMethods = classEnv.builtinMethods;
		} in 
		let _ = setEnvParams func_decl.fformals env
		in 
		{
			sfscope = func_decl.fscope;
			sfname = func_decl.fname;
			sfformals = List.map (fun f -> convertFormalToSast f env) func_decl.fformals; 
			sfreturn = func_decl.freturn;
			sfbody = List.map (fun s -> convertStmtToSast s env) func_decl.fbody;
		}

	in
	(* Semantic checking for class constructor *)
	let checkConstructor constructor classEnv = 
(*		let formalTypes = getListOfFormalTypes constructor
		in
		let checking =
			
			if(StringMap.mem (strOfFormals formalTypes) classEnv.classMap.constructorMap)
				then raise (Failure("Duplicate Constructor Definition"))
			else if ((List.length formalTypes <> 0) && (hasDuplicateFormalNames constructor.fformals = true))
				then raise(Failure("Formal names must be unique"))  
		in checking;
 	        {
                	mscope = constructor.fscope;
                        mname  = constructor.fname;
                        mformalTypes = formalTypes;
			mReturn = JVoid;
                }
*)
		{
                mscope = Public;
                mname = "";
                mformalTypes = [];
                mReturn = JInt
        	}
	in
	let convertConstructorToSast constructor classEnv =
		let constructorSignature = checkConstructor constructor classEnv
		in
		let _ = classEnv.classMap.constructorMap <- StringMap.add (strOfFormals constructorSignature.mformalTypes) constructorSignature classEnv.classMap.constructorMap
		in
		let env = {
			envClassName = classEnv.className;
			envClassMaps = classEnv.classMaps;
			envClassMap  = classEnv.classMap;
			envLocals    = StringMap.empty;
			envParams    = StringMap.empty;
			envReturnType= constructor.freturn;
			envBuiltinMethods = classEnv.builtinMethods;
		} in
		let _ = setEnvParams constructor.fformals env
		in 
		{
			sfscope = constructor.fscope;
			sfname = constructor.fname;
			sfformals = List.map (fun f -> convertFormalToSast f env) constructor.fformals; 
			sfreturn = constructor.freturn;
			sfbody = List.map (fun s -> convertStmtToSast s env) constructor.fbody;
		}
	
	in
	(* Sematic checking for class variable *)
	let checkVdecl vdecl env = 
		"placeholder"
(*		let check = 
			if StringMap.mem vdecl.vname env.envClassMap.variableMap
				then raise (Failure("Variable name already used"))
			else if vdecl.vexpr <> Ast.Noexpr && getType vdecl.vexpr env <> vdecl.vtype
				then raise (Failure(str_of_expr vdecl.vexpr ^ " is of type " ^ str_of_type (getType vdecl.vexpr env) ^ " but type " ^ str_of_type vdecl.vtype ^ " is expected"))
		in check
*)	in

	let convertVariableToSast vdecl classEnv =
		let env = {
                        envClassName = classEnv.className;
                        envClassMaps = classEnv.classMaps;
                        envClassMap  = classEnv.classMap;
                        envLocals    = StringMap.empty;
                        envParams    = StringMap.empty;
                        envReturnType= JVoid; (*@TODO make sure return is not possible here*)
			envBuiltinMethods = classEnv.builtinMethods;
                } in
		let _ = checkVdecl vdecl env
		in
		let _ =  classEnv.classMap.variableMap <- StringMap.add vdecl.vname vdecl classEnv.classMap.variableMap
	in {
                        svscope = vdecl.vscope;
                        svtype  = vdecl.vtype;
                        svname  = vdecl.vname;
                        svexpr  = convertExprToSast vdecl.vexpr env;
                }
	in
	let convertCbodyToSast cbody classEnv =
	{
		svariables = List.map (fun v -> convertVariableToSast v classEnv) (List.rev cbody.variables);
        sconstructors = List.map (fun cst -> convertConstructorToSast cst classEnv) (List.rev cbody.constructors);
		smethods = List.map (fun m -> convertMethodToSast m classEnv) (List.rev cbody.methods);
    }

	in
	let checkClass class_decl classEnv = 
		let firstChar = String.get class_decl.cname 0
		in
		let lowerChar = Char.lowercase firstChar
		in 
		let checking =  
			if lowerChar = firstChar
			 	then raise (Failure ("Class name not capitalized: " ^ class_decl.cname))
			else if StringMap.mem class_decl.cname classEnv.classMaps
				then raise (Failure ("Duplicate Class Name: " ^ class_decl.cname))
			in checking 	
	in
	let convertClassToSast class_decl classEnv =
		classEnv.className <- class_decl.cname;
		checkClass class_decl classEnv;
		let classMap = {
			variableMap = StringMap.empty;
			constructorMap = StringMap.empty;
			methodMap = StringMap.empty;
		} in
		classEnv.classMap <- classMap;
		let result =
		{
			scscope = class_decl.cscope;
			scname  = class_decl.cname;
			scbody  = convertCbodyToSast class_decl.cbody classEnv;
		} in
		classEnv.classMaps <- StringMap.add classEnv.className classEnv.classMap classEnv.classMaps;
		result

	in
	let classEnv = {
		className = "";
		classMaps = StringMap.empty;
		classMap  = { 
		variableMap = StringMap.empty;
		constructorMap = StringMap.empty;
		methodMap = StringMap.empty;
		};
		builtinMethods = builtinMethods;
	}
	in
	let get_classes =
		List.map (fun c -> convertClassToSast c classEnv) classes
	in
	let sprogram = 
	{
		classes = get_classes;
		functions = get_methods_minus_main get_classes;
		main = get_main get_classes;
		reserved = [];
	}
	in
	sprogram

 (* Translates Ast to Sast *)
let check program = match program with
         Program (classes) -> ignore (createClassIndices classes); convertToSast classes

