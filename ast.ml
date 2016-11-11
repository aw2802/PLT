
(* Possible data types *)
type data_type =
  | JChar 
  | JVoid
  | JBoolean
  | Object of string 

  
(* Operators *)
type op = Plus | Minus | Divide | Times | Eq | Neq | Lt | Leq | Gt | Geq |
OR | AND | NOT 
(* removed assign from op list, may need to add back *)

type scope = Private | Public

type formal = Formal of data_type * string 

(* Variable Declarations *)
type vdecl = {
  vscope: scope;
  vtype: data_type;
  vname: string;
}

(* Expressions *)
type expr =
	  Int_Lit of int
	| Float_Lit of float
	| Char_Lit of string
	| Noexpr
	| Binop of expr * op * expr 
	| Assign of string * expr
	| Null 

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
  fscope : scope;
  fname : string; (* name of the function *)
  fformals : formal list; (* formal params *)
  freturn : data_type; (* return type *)
  fbody : stmt list; (* statements, including local variable declarations *)
}

type cbody = {
  variables : vdecl list;
  constructors : func_decl list;
  methods : func_decl list;
}

type class_decl =  {
    cscope : scope; 
    cname : string;
    cbody : cbody;
}

type program = Program of class_decl list 

