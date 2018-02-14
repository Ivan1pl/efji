open Lexing
open Syntax

let usage () =
  Printf.eprintf "Usage: %s file ...\n" Sys.argv.(0);
  exit 1

let error msg =
  Printf.eprintf "%s: %s\n" (Source.location ()) msg

exception ParseError of (exn * (int * int * string))

let parse () =
  try
    let _ = Parsing.set_trace false in
    Source.with_lexbuf
      (fun lexbuf ->
        try
          Parser.goal Lexer.token lexbuf
        with e ->
          begin
            (match e with
            | Failure str -> print_endline str
            | _           -> ());
            let curr = lexbuf.Lexing.lex_curr_p in
            let line = curr.Lexing.pos_lnum in
            let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
            let tok = Lexing.lexeme lexbuf in
            raise (ParseError (e,(line,cnum,tok)))
          end)
  with ParseError (_, (line, cnum, tok)) ->
    begin
      print_endline "Parse error";
      print_endline ("In line: " ^ string_of_int line);
      print_endline ("On position: " ^ string_of_int cnum);
      print_endline ("On token: " ^ tok);
      failwith "Error";
    end
  (*with e -> failwith "Error"*)
  (* with e -> error (Printexc.to_string e) *)

let write_class name bytes =
  let out = open_out_bin (name ^ ".class")
  in output_bytes out bytes; close_out out

let process_program program =
  Syntax.pretty_printer program;
  match program with
  | Program (cl, es) -> try
                          List.iter (Typing.class_ok cl) cl;
                          List.iter (fun e ->
                                     print_endline "================================================================================";
                                     print_string "Expression: "; Syntax.pretty_printer_expr e; print_endline "";
                                     try
                                       let t = Typing.jExpr_type cl e Typing.empty_env
                                       in print_string "Result type: "; Syntax.pretty_printer_type t; print_endline ""; print_string "Result: ";
                                          Syntax.pretty_printer_expr (Interp.eval cl e); print_endline ""
                                     with _ -> ()) es
                        with e ->
                          match e with
                          | Failure s -> print_string "Error: "; print_endline s
                          | _         -> ()

let rec process_programs programs =
  match programs with
  | []            -> print_endline ""
  | program :: xs -> process_program program; process_programs xs

let _ =
  let argc = Array.length Sys.argv in
  if argc <= 1 then
    usage ();
  let programs = ref [] in
  begin
    for i = 1 to argc-1 do
      Source.set_file_name Sys.argv.(i);
      programs := (parse ()) :: !programs;
    done;
    process_programs !programs;
  end
  
