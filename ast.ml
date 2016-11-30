
(* Possible data types *)
type data_type =
  | JChar 
  | JVoid
  | JBoolean
  | JFloat
  | JInt
  | Object of string 

  
(* Operators *)
type op = Add | Sub | Div | Mult | Equal | Neq | Less | Leq | Greater | Geq |
Or | And | Not
(* removed assign from op list, may need to add back *)

type scope = Private | Public

type formal = {
  fvtype: data_type;
  fvname: string;
}

(* Expressions *)
type expr =
    Id of string
	| Int_Lit of int
	| Float_Lit of float
	| Char_Lit of char
  | Bool_Lit of bool
  | String_Lit of string
	| Noexpr
	| Binop of expr * op * expr 
  | Unop of op * expr
	| Assign of expr * expr
  | FuncCall of string * expr list  
	| Null 

  (* Variable Declarations *)
type vdecl = {
  vscope: scope;
  vtype: data_type;
  vname: string;
  vexpr: expr;
}

(* Statements *)
type stmt =
  Block of stmt list
| Expr of expr
| VarDecl of vdecl 
| LocalVarDecl of data_type * string * expr
| Return of expr
| If of expr * stmt * stmt
| For of expr * expr * expr * stmt
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

