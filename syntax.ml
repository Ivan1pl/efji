type ident = { id : string }

let ident s = { id = s }

type jType =
  | Named of ident
  | Int
  | Boolean

type jVar =
  | Var of jType * ident

type jAop =
  | Plus
  | Minus
  | Times
  | Div
  | Mod

type jCop =
  | Lt
  | Leq
  | Gt
  | Geq

type jOCop =
  | Eq
  | Neq

type jBConst =
  | JTrue
  | JFalse

type jBop =
  | And
  | Or

type jExpr =
  | This
  | Val of ident
  | Call of jExpr * ident * jExpr list
  | Field of jExpr * ident
  | New of ident * jExpr list
  | Num of int
  | Aop of jAop * jExpr * jExpr
  | UnaryMinus of jExpr
  | Cop of jCop * jExpr * jExpr
  | OCop of jOCop * jExpr * jExpr
  | BConst of jBConst
  | Bop of jBop * jExpr * jExpr
  | Not of jExpr
  | Null
  | Cast of jType * jExpr
  | If of jExpr * jExpr * jExpr

type jConstructor =
  | Constructor of ident * (* args *) jVar list * (* super args *) ident list * (* assignments *) ident list

type jMethod =
  | Method of jType * ident * jVar list * jExpr

type jClass =
  | Class of ident * ident * jVar list * jConstructor * jMethod list

type jProgram =
  | Program of jClass list * jExpr list

let val_to_type v =
  match v with
  | Val n -> if n.id = "int" then Int else if n.id = "boolean" then Boolean else Named n
  | _     -> failwith "Type name expected in cast"

let pretty_printer_type t =
  match t with
  | Named i -> print_string i.id
  | Int     -> print_string "int"
  | Boolean -> print_string "boolean"

let pretty_printer_aop o =
  match o with
  | Plus  -> print_string "+"
  | Minus -> print_string "-"
  | Times -> print_string "*"
  | Div   -> print_string "/"
  | Mod   -> print_string "%"

let pretty_printer_cop o =
  match o with
  | Lt  -> print_string "<"
  | Leq -> print_string "<="
  | Gt  -> print_string ">"
  | Geq -> print_string ">="

let pretty_printer_ocop o =
  match o with
  | Eq  -> print_string "=="
  | Neq -> print_string "!="

let pretty_printer_bop o =
  match o with
  | And -> print_string "&&"
  | Or  -> print_string "||"

let pretty_printer_bconst c =
  match c with
  | JTrue  -> print_string "true"
  | JFalse -> print_string "false"

let rec pretty_printer_expr e =
  match e with
  | This              -> print_string "this"
  | Val i             -> print_string i.id
  | Call (e, i, args) -> print_string "("; pretty_printer_expr e; print_string ")."; print_string i.id; print_string "(";
                         pretty_printer_args args; print_string ")"
  | Field (e, i)      -> print_string "("; pretty_printer_expr e; print_string ")."; print_string i.id
  | New (i, args)     -> print_string "new "; print_string i.id; print_string "(";
                         pretty_printer_args args; print_string ")"
  | Num n             -> print_int n
  | Aop (o, e1, e2)   -> print_string "("; pretty_printer_expr e1; print_string ") "; pretty_printer_aop o; print_string " ("; pretty_printer_expr e2; print_string ")"
  | UnaryMinus e      -> print_string "- "; pretty_printer_expr e
  | Cop (o, e1, e2)   -> pretty_printer_expr e1; print_string " "; pretty_printer_cop o; print_string " "; pretty_printer_expr e2
  | OCop (o, e1, e2)  -> pretty_printer_expr e1; print_string " "; pretty_printer_ocop o; print_string " "; pretty_printer_expr e2
  | BConst c          -> pretty_printer_bconst c
  | Bop (o, e1, e2)   -> print_string "("; pretty_printer_expr e1; print_string ") "; pretty_printer_bop o; print_string " ("; pretty_printer_expr e2; print_string ")"
  | Not e             -> print_string "!("; pretty_printer_expr e; print_string ")"
  | Null              -> print_string "null"
  | Cast (t, e)       -> print_string "("; pretty_printer_type t; print_string ") "; pretty_printer_expr e
  | If (e, e1, e2)    -> print_string "if ("; pretty_printer_expr e; print_string ") "; pretty_printer_expr e1; print_string " else "; pretty_printer_expr e2
and pretty_printer_args args =
  match args with
  | []      -> ()
  | e :: es -> pretty_printer_expr e; (if List.length es = 0 then () else print_string ", "); pretty_printer_args es

let pretty_printer_var v =
  match v with
  | Var (t, i) -> print_string "    "; pretty_printer_type t; print_string " "; print_string i.id; print_endline ";"

let rec pretty_printer_vars vs =
  match vs with
  | []      -> ()
  | v :: vs -> pretty_printer_var v; pretty_printer_vars vs

let rec pretty_printer_sargs sargs =
  match sargs with
  | []         -> ()
  | i :: sargs -> print_string i.id; (if List.length sargs = 0 then () else print_string ", ");
                  pretty_printer_sargs sargs

let rec pretty_printer_asgns asgns =
  match asgns with
  | []         -> ()
  | i :: asgns -> print_string "        this."; print_string i.id; print_string " = "; print_string i.id; print_endline ";";
                  pretty_printer_asgns asgns

let pretty_printer_arg v b =
  match v with
  | Var (t, i) -> pretty_printer_type t; print_string " "; print_string i.id;
                  if b then () else print_string ", "

let rec pretty_printer_args vs =
  match vs with
  | [] -> ()
  | v :: vs -> pretty_printer_arg v (List.length vs = 0); pretty_printer_args vs

let pretty_printer_constructor c =
  match c with
  | Constructor (i, vs, sargs, asgns) -> print_string "    "; print_string i.id; print_string "(";
                                         pretty_printer_args vs; print_endline ") {"; print_string "        super(";
                                         pretty_printer_sargs sargs; print_endline ");"; pretty_printer_asgns asgns;
                                         print_endline "    }"

let pretty_printer_method m =
  match m with
  | Method (t, i, vs, e) -> print_string "    "; pretty_printer_type t; print_string " "; print_string i.id; print_string "(";
                            pretty_printer_args vs; print_endline ") {"; print_string "        return "; pretty_printer_expr e; print_endline ";";
                            print_endline "    }"

let rec pretty_printer_methods ms =
  match ms with
  | []      -> ()
  | m :: ms -> pretty_printer_method m; pretty_printer_methods ms

let pretty_printer_class c =
  match c with
  | Class (i1, i2, vs, c, ms) -> print_string "class "; print_string i1.id; print_string " extends "; print_string i2.id; print_endline " {";
                                 pretty_printer_vars vs; pretty_printer_constructor c; pretty_printer_methods ms;
                                 print_endline "}"

let rec pretty_printer_classes cl =
  match cl with
  | []      -> print_endline ""
  | c :: cs -> pretty_printer_class c; pretty_printer_classes cs

let pretty_printer_program program =
  match program with
  | Program (cl, es) -> pretty_printer_classes cl; List.iter (fun e -> pretty_printer_expr e; print_endline "") es

let pretty_printer program =
  print_endline "#################### Program ####################";
  pretty_printer_program program

