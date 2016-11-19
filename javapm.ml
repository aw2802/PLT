open Ast
open Sast

let _ =
	let filename = Sys.argv.(1) ^ ".javapm" in

	let in_channel = open_in Sys.argv.(1) in

	let lexbuf = Lexing.from_channel in_channel in

	let ast  = Parser.program Scanner.token lexbuf in
(**	let sast = Semant.check ast in
	let outprog = Codegen.translate sast in
	print_string (outprog);;**)
	print_string ("hi");
