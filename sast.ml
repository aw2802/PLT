open Ast

type svdecl = {
	svscope: scope;
	svtype: data_type;
	svname: string;
}

type sformal = {
	sformal_type: data_type;
	sformal_name: string;
}

type sexpr = 
	  SInt_Lit of int
	| SBoolean_Lit of bool
	| SFloat_Lit of float
	| SString_Lit of string
	| SChar_Lit of char
	| SNull
	| SId of string * data_type
	| SBinop of sexpr * op * sexpr * data_type
	| SAssign of sexpr * sexpr * data_type
	| SNoexpr
	| SFuncCall of string * sexpr list * data_type * int (**check me y tho**)
	| SUnop of op * sexpr * data_type
(**	| SArrayCreate of data_type * sexpr list * data_type
	| SArrayAccess of sexpr * sexpr list * data_type
	| SArrayElements
	| SObjAccess of sexpr * sexpr * data_type
	| SObjectCreate **)

type sstmt = 
	  SBlock of sstmt list
	| SExpr of sexpr * data_type
	| SVarDecl of vdecl * data_type * string (**fix meeeee **)
	| SReturn of sexpr * data_type
	| SIf of sexpr * sstmt * sstmt 
	| SFor of sexpr * sexpr * sexpr * sstmt 
	| SWhile of sexpr * sstmt

type func_type = User | Reserved

type sfunc_decl = {
	sfscope: scope;
	sfname: string;
	sfformals: sformal list;
	sfreturn: data_type;
	sfbody: sstmt list;
	functype: func_type;
}

type scbody = {
	svariables: svdecl list;
	sconstructors: sfunc_decl list; (**@TODO**)
	smethods: sfunc_decl list; 
}


type sclass_decl = {
	scscope: scope;
	scname: string;
	scbody: scbody;
}

type sprogram = {
	classes: sclass_decl list;
	functions: sfunc_decl list;
	main: sfunc_decl;
	reserved: sfunc_decl list;
}
