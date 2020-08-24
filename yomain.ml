open Mainfile
let _ =
  try
  let in_stream = open_in Sys.argv.(4) in
    let lexbuf = Lexing.from_channel in_stream in
    while true do
      let result = Real_parser.main Real_lexer.token lexbuf in
       print_matrix result; print_newline(); flush stdout
    done;
    close_in in_stream;
  with 
  | Real_lexer.EOF ->exit 0
  | Real_lexer.Invalid_input->Printf.printf "invalid_input"; exit 0;