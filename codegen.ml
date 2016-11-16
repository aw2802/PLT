(* 
LLVM Code Generator
*)

open Llvm
open Hashtbl

open Ast
exception Error of string

let context = global_context ()
let the_module = create_module context "javapm"
let builder = builder context

let stubby parameter = function
	_ -> "stubed"