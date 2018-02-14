(* Java reserved words. *)

open Parser

let hash_table list =
  let tbl = Hashtbl.create (List.length list)
  in
  List.iter (fun (s, t) -> Hashtbl.add tbl s t) list;
  tbl

let words = hash_table [
  "boolean", BOOLEAN;
  "class", CLASS;
  "else", ELSE;
  "extends", EXTENDS;
  "if", IF;
  "int", INT;
  "new", NEW;
  "return", RETURN;
  "super", SUPER;
  "this", THIS;
  "while", WHILE;
  "true", TRUE;
  "false", FALSE;
  "null", NULL;
]

let lookup name =
  try Some (Hashtbl.find words name) with Not_found -> None
