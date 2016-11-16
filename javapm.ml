
let filename = Sys.argv.(1) ^ ".javapm" in

let in_channel = open_in Sys.argv.(1) in

let lexbuf = Lexing.from_channel in_channel in

let program = Parser.program Scanner.token lexbuf in
let outprog = Codegen.stubby program in

let headers = outprog 
		in Printf.fprintf (open_out filename)  headers