open Ast
open Sast

let classIndices: (string, int) Hashtbl.t =  Hashtbl.create 100

  
let createClassIndices cdecls=  
	let classHandler index cdecl=
	Hashtbl.add classIndices cdecl.cname index in (*scope handling is missing*)
	List.iteri classHandler cdecls

let isMain f = f.sfname = "main"

let get_methods l classy = List.concat [classy.scbody.smethods;l]

let get_main m = List.hd (List.filter isMain (List.fold_left get_methods [] m))

	let convertToSast classes =
		
			let convertFormalToSast formal =
				{sformal_type = formal.fvtype;
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
				| Assign(expr1, expr2)		-> SAssign(convertExprToSast expr1, convertExprToSast expr2, JInt)
				| FuncCall(s, el)		-> SFuncCall(s, (List.map convertExprToSast el), JInt, 1)
				| Unop(op, expr)		-> SUnop(op, convertExprToSast expr, JInt)

			in
			let convertVdeclToSast vdecl = 
			{
				svscope = vdecl.vscope;
			 	svtype  = vdecl.vtype;
			 	svname  = vdecl.vname;
			 	svexpr = convertExprToSast vdecl.vexpr;
			}
			in
			let rec convertStmtToSast stmt = match stmt with
				  Block(sl)			-> SBlock(List.map convertStmtToSast sl)
				| Expr(expr)			-> SExpr(convertExprToSast expr, JInt)
				| VarDecl(vdecl)		-> SVarDecl(convertVdeclToSast vdecl)
				| Return(expr)  		-> SReturn(convertExprToSast expr, JInt)
				| If(expr, stmt1, stmt2)	-> SIf(convertExprToSast expr, convertStmtToSast stmt1, convertStmtToSast stmt2)
				| For(expr1, expr2, expr3, stmt)-> SFor(convertExprToSast expr1, convertExprToSast expr2, convertExprToSast expr3, convertStmtToSast stmt)
				| While(expr, stmt)		-> SWhile(convertExprToSast expr, convertStmtToSast stmt)
	
		in
		let convertMethodToSast func_decl = 
			{sfscope = func_decl.fscope;
			 sfname = func_decl.fname;
			 sfformals = List.map convertFormalToSast func_decl.fformals; 
			 sfreturn = func_decl.freturn;
			 sfbody = List.map convertStmtToSast func_decl.fbody;
			}
		in
		let convertCbodyToSast cbody =
                        {svariables = List.map convertVdeclToSast cbody.variables;
                         sconstructors = List.map convertMethodToSast cbody.constructors;
			 smethods = List.map convertMethodToSast cbody.methods;
                        }

		in
		let convertClassToSast class_decl =
			{scscope = class_decl.cscope;
			 scname  = class_decl.cname;
			 scbody	 = convertCbodyToSast class_decl.cbody;
			} 

		in
		let get_classes = List.map convertClassToSast classes in
		let sprogram = 
			{classes = get_classes;
			 functions = [];
			 main = get_main get_classes;
			 reserved = [];
			}
		in
		sprogram

 (* Translates Ast to Sast *)
let check program = match program with
         Program (classes) -> convertToSast classes

