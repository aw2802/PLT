
  

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.token lexbuf in
  print_endline (string_of_int expr)