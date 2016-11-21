open Ast
open Sast

let _ =
	let filename = Sys.argv.(1) ^ ".javapm" in

	let in_channel = open_in Sys.argv.(1) in

	let lexbuf = Lexing.from_channel in_channel in
	let program = parser.program scanner.token lexbuf in

	let finalcast =  Semant.check program in
	let outprog = Codegen.translate finalcast in


