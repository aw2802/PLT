
(* Possible data types *)
type data_type =
  | JChar 
  | JVoid
  | JBoolean
  
(* Operators *)
type op = Plus | Minus | Divide | Times | Assign | Eq | Neq | Lt | Leq | Gt | Geq |
OR | AND | NOT

(* Variable Declarations *)
type vdecl = {
  vtype: data_type;
  vname: string;
}

(* Expressions *)
type expr =
	Int_Lit of int
	| Float_Lit of float
	| Char_Lit of string

(* Statements *)
type stmt =
  Block of stmt list
| Expr of expr
| VarDecl of vdecl
| Return of expr
| If of expr * stmt * stmt
| For of stmt * expr * expr * stmt
| While of expr * stmt

(* Functions *)
type func_decl = {
  fname : string; (* name of the function *)
  fformals : vdecl list; (* formal params *)
  freturn : data_type; (* return type *)
  fbody : stmt list; (* statements, including local variable declarations *)
}

type program = stmt list * func_decl list

