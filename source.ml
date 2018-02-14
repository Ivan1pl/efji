let file = ref ""

let line = ref 0

let set_file_name f =
  file := f;
  line := 1

let next_line buf =
  line := !line + 1

let location () = Printf.sprintf "file %s, line %d" !file !line

let lexeme_pos buf = Lexing.lexeme_start buf

let with_lexbuf f =
  let chan = open_in !file in
  let cleanup () =
    close_in chan;
    Parsing.clear_parser ()
  in
  try
    let result = f (Lexing.from_channel chan) in
    cleanup ();
    result
  with e -> (cleanup (); raise e)
