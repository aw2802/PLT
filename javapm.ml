open Ast
open Sast

let _ =
	let filename = Sys.argv.(1) ^ ".javapm" in

	let in_channel = open_in Sys.argv.(1) in

	let lexbuf = Lexing.from_channel in_channel in

	let ast  = Parser.program Scanner.token lexbuf in
	print_string("parsed and scanned");

	let sast = Semant.check ast in
	let outprog = print_string("semantic check done"); Codegen2.translate sast in
	Llvm_analysis.assert_valid_module outprog;
	print_string (Llvm.string_of_llmodule outprog);;
