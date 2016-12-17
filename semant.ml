open Ast
open Sast

module StringMap = Map.Make(String)

let classIndices: (string, int) Hashtbl.t =  Hashtbl.create 10
  
let createClassIndices cdecls=  
	let classHandler index cdecl=
	Hashtbl.add classIndices cdecl.cname index in (*scope handling is missing*)
	List.iteri classHandler cdecls

type methodSignature = {
	mscope: scope;
	mname : string;
	mformalTypes: data_type list; 
}

type classMap = {
        variableMap: Ast.vdecl StringMap.t;
        constructorMap: methodSignature StringMap.t;
        methodMap:  methodSignature StringMap.t;
}

type classEnv = {
	className: string;
	classMaps: classMap StringMap.t;
	classMap: classMap;
}

type env = {
        envClassName: string;
        envClassMaps: classMap StringMap.t;
        envClassMap: classMap;
        envLocals: Ast.vdecl StringMap.t;
        envParams: Ast.formal StringMap.t;
        envReturnType: data_type;
}

let updateEnv env envName = {
	envClassName = envName;
	envClassMaps = env.envClassMaps;
	envClassMap  = env.envClassMap;
	envLocals    = env.envLocals;
	envParams    = env.envParams;
	envReturnType = env.envReturnType;
}
	
let isMain f = f.sfname = "main"

let get_methods l classy = List.concat [classy.scbody.smethods;l]

let get_main m = List.hd (List.filter isMain (List.fold_left get_methods [] m))

let get_methods_minus_main m = snd (List.partition isMain (List.fold_left get_methods [] m))

let typOFSexpr = function
		SInt_Lit(i)			-> JInt	
	| 	SBoolean_Lit(b)			-> JBoolean	
	| 	SFloat_Lit(f)			-> JFloat
	| 	SString_Lit(s) 			-> JString
	| 	SChar_Lit(c) 			-> JChar
	| 	SId(_, d) 			-> d
	| 	SBinop(_, _, _, d) 		-> d
	|   	SAssign(_, _, d) 		-> d
	| 	SArrayCreate(_, _, d)	-> d
	| 	SArrayAccess(_, _, d) 	-> d
	| 	SFuncCall(_, _, d,_)	-> d
	|  	SUnop(_, _, d) 			-> d 
	| 	SCreateObject(_,_,d)	-> d
	| 	SObjAccess(_,_,d) -> d

let convertToSast classes =
	
		let convertFormalToSast formal env =
		{
			sformal_type = formal.fvtype;
		 	sformal_name = formal.fvname;
		}
		in
		let rec convertExprToSast expr = match expr with
			  Int_Lit(i)	-> SInt_Lit(i)
			| Bool_Lit(b)	-> SBoolean_Lit(b)
			| Float_Lit(f)	-> SFloat_Lit(f)
			| String_Lit(s)	-> SString_Lit(s)
			| Char_Lit(c)	-> SChar_Lit(c)
			| Null		-> SNull
			| Noexpr	-> SNoexpr
			| Id(id)	-> SId(id, JInt) (** @TODO Sast has SId(string, datatype) **)
			| Binop(expr1, op, expr2)	-> SBinop(convertExprToSast expr1, op, convertExprToSast expr2, JInt) (** @TODO Not sure about the data_type value. Same below **) 	
			| ArrayCreate(d, el)  -> SArrayCreate(d, (List.map convertExprToSast el), JInt) (* @TODO *)
			| ArrayAccess(e, el)  -> SArrayAccess(convertExprToSast e, (List.map convertExprToSast el), JInt) (* @TODO *)
			| Assign(id, e)		-> SAssign(id, convertExprToSast e, JInt)
			| FuncCall(s, el)		-> SFuncCall(s, (List.map convertExprToSast el), JInt, 1)
			| Unop(op, expr)		-> SUnop(op, convertExprToSast expr, JInt)
			| CreateObject(s,el)	-> SCreateObject(s, (List.map convertExprToSast el), Object(s))
			| ObjAccess(e1,e2) -> SObjAccess(convertExprToSast e1, convertExprToSast e2, JInt) (* Just a placeholder *)
			| TupleCreate(dl, el)	-> STupleCreate(dl, (List.map convertExprToSast el), Tuple(dl))
			| TupleAccess(e1, e2)	-> STupleAccess(convertExprToSast e1, convertExprToSast e2, JInt) (* @TODO *)		
	in
		let convertVdeclToSast vdecl = 
		{
			svscope = vdecl.vscope;
		 	svtype  = vdecl.vtype;
		 	svname  = vdecl.vname;
		 	svexpr 	= convertExprToSast vdecl.vexpr;
		}
		in
		let rec convertStmtToSast stmt env = match stmt with
			  Block(sl)			-> SBlock(List.map (fun s -> convertStmtToSast s env) sl)
			| Expr(expr)			-> SExpr(convertExprToSast expr, JInt)
			| VarDecl(vdecl)		-> SVarDecl(convertVdeclToSast vdecl)
			| LocalVarDecl(dt, id, expr)	-> SLocalVarDecl(dt, id, convertExprToSast expr)
			| Return(expr)  		-> SReturn(convertExprToSast expr, JInt)
			| If(expr, stmt1, stmt2)	-> SIf(convertExprToSast expr, convertStmtToSast stmt1 env, convertStmtToSast stmt2 env)
			| For(expr1, expr2, expr3, stmt)-> SFor(convertExprToSast expr1, convertExprToSast expr2, convertExprToSast expr3, convertStmtToSast stmt env)
			| While(expr, stmt)		-> SWhile(convertExprToSast expr, convertStmtToSast stmt env)

	in
	let checkMethod func_decl env =
		if StringMap.mem func_decl.fname env.classMap.methodMap
			then raise (Failure("Duplicate Method Name: " ^ func_decl.fname));
(*		let rec check func_decl.fformals.fvname = match func_decl.fformals.fvname with
			(h::t) -> let x = (List.mem h t) in
				  if x 
					then raise (Failure("Duplicate Parameter Names: "^ h))
		in check
 *)
		let signature = {
			mscope = func_decl.fscope;
			mname = func_decl.fname;
			mformalTypes = List.map (fun fl -> fl.fvtype) func_decl.fformals;			
		} in signature 
	in
	let convertMethodToSast func_decl classEnv =
		let env = {
			envClassName = classEnv.className;
			envClassMaps = classEnv.classMaps;
			envClassMap  = classEnv.classMap;
			envLocals    = StringMap.empty;
			envParams    = StringMap.empty;
			envReturnType= func_decl.freturn;
		} in 
		let methodSignature = checkMethod func_decl classEnv
		in
		let result =
		{
			sfscope = func_decl.fscope;
			sfname = func_decl.fname;
			sfformals = List.map (fun f -> convertFormalToSast f env) func_decl.fformals; 
			sfreturn = func_decl.freturn;
			sfbody = List.map (fun s -> convertStmtToSast s env) func_decl.fbody;
		} in
		classEnv.classMap.methodMap = StringMap.add func_decl.fname methodSignature classEnv.classMap.methodMap;
		result
	in
	let convertVariableToSast vdecl classEnv = 
		convertVdeclToSast vdecl
	in
	let convertCbodyToSast cbody classEnv =
	{
		svariables = List.map (fun v -> convertVariableToSast v classEnv) cbody.variables;
        	sconstructors = List.map (fun cst -> convertMethodToSast cst classEnv) cbody.constructors;
		smethods = List.map (fun m -> convertMethodToSast m classEnv) cbody.methods;
    	}

	in
	let checkClass class_decl classEnv = 
		let firstChar = String.get class_decl.cname 0
		in
		let lowerChar = Char.lowercase firstChar
		in 
		let checking =  
			if lowerChar = firstChar
			 	then raise (Failure ("Class Name not capitailized: " ^ class_decl.cname))
			else if StringMap.mem class_decl.cname classEnv.classMaps
				then raise (Failure ("Duplicate Class Name: " ^ class_decl.cname))
			in checking 	
	in
	let convertClassToSast class_decl classEnv =
		classEnv.className = class_decl.cname;
		checkClass class_decl classEnv;
		let classMap = {
			variableMap = StringMap.empty;
			constructorMap = StringMap.empty;
			methodMap = StringMap.empty;
		} in
		classEnv.classMap = classMap;
		let result =
		{
			scscope = class_decl.cscope;
			scname  = class_decl.cname;
			scbody  = convertCbodyToSast class_decl.cbody classEnv;
		} in
		classEnv.classMaps = StringMap.add classEnv.className classEnv.classMap classEnv.classMaps;
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

