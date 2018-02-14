open Syntax
open Typing

exception ReductionError

let rec subst vl vr e =
  match e with
  | This              -> if vr.id = "this" then vl else This
  | Val i             -> if i = vr then vl else Val i
  | Call (e, i, args) -> Call ((subst vl vr e), i, List.map (subst vl vr) args)
  | Field (e, i)      -> Field (subst vl vr e, i)
  | New (i, args)     -> New (i, List.map (subst vl vr) args)
  | Num n             -> Num n
  | Aop (o, e1, e2)   -> Aop (o, subst vl vr e1, subst vl vr e2)
  | UnaryMinus e      -> UnaryMinus (subst vl vr e)
  | Cop (o, e1, e2)   -> Cop (o, subst vl vr e1, subst vl vr e2)
  | OCop (o, e1, e2)  -> OCop (o, subst vl vr e1, subst vl vr e2)
  | BConst c          -> BConst c
  | Bop (o, e1, e2)   -> Bop (o, subst vl vr e1, subst vl vr e2)
  | Not e             -> Not (subst vl vr e)
  | Null              -> Null
  | Cast (t, e)       -> Cast (t, subst vl vr e)
  | If (e, e1, e2)    -> If (subst vl vr e, subst vl vr e1, subst vl vr e2)

let rec subst_l vls vrs e =
  match vls with
  | []        -> (match vrs with
                 | []        -> e
                 | vr :: vrs -> failwith "Incorrect number of values")
  | vl :: vls -> (match vrs with
                 | []        -> failwith "Incorrect number of values"
                 | vr :: vrs -> subst_l vls vrs (subst vl vr e))

let rec eval classes e =
  let rec get_field args idents i =
    match args with
    | []      -> failwith ("Field " ^ i.id ^ " not found")
    | x :: xs -> (match idents with
                 | []      -> failwith ("Field " ^ i.id ^ " not found")
                 | y :: ys -> if y = i then x else get_field xs ys i)
  in let eval_aop o v1 v2 =
    match o with
    | Plus  -> Num (v1 + v2)
    | Minus -> Num (v1 - v2)
    | Times -> Num (v1 * v2)
    | Div   -> if v2 = 0 then failwith "Division by 0" else Num (v1 / v2)
    | Mod   -> if v2 = 0 then failwith "Division by 0" else Num (v1 mod v2)
  in let eval_bop o c1 c2 =
    let b1 = (match c1 with
             | JTrue  -> true
             | JFalse -> false)
    in let b2 = (match c2 with
                | JTrue  -> true
                | JFalse -> false)
    in match o with
    | And -> BConst (if b1 && b2 then JTrue else JFalse)
    | Or  -> BConst (if b1 || b2 then JTrue else JFalse)
  in let eval_cop o v1 v2 =
    match o with
    | Lt  -> BConst (if v1 < v2 then JTrue else JFalse)
    | Leq -> BConst (if v1 <= v2 then JTrue else JFalse)
    | Gt  -> BConst (if v1 > v2 then JTrue else JFalse)
    | Geq -> BConst (if v1 >= v2 then JTrue else JFalse)
  in let eval_ocop o v1 v2 =
    match o with
    | Eq  -> BConst (if v1 = v2 then JTrue else JFalse)
    | Neq -> BConst (if v1 = v2 then JFalse else JTrue)
  in let eval_cast classes t v =
    match t with
    | Int     -> (match v with
                 | Num n         -> Num n
                 | BConst JTrue  -> Num 1
                 | BConst JFalse -> Num 0
                 | _             -> failwith "ClassCastException")
    | Boolean -> (match v with
                 | Num n    -> BConst (if n = 0 then JFalse else JTrue)
                 | BConst c -> BConst c
                 | _        -> failwith "ClassCastException")
    | Named d -> (match v with
                 | Null       -> Null
                 | New (c, _) -> if is_subtype classes (Named c) (Named d) then v else failwith "ClassCastException"
                 | _          -> failwith "ClassCastException")
  in try
    match e with
    | This              -> failwith ("Uninitialized variable this")
    | Val i             -> failwith ("Uninitialized variable " ^ i.id)
    | Call (e, i, args) -> (match eval classes e with
                           | New (c, cargs) -> let (x, e) = mbody classes i c
                                               in eval classes (subst (New (c, cargs)) (ident "this") (subst_l (List.map (eval classes) args) x e))
                           | Null           -> failwith "NullPointerException"
                           | _              -> failwith "Unable to reduce expression - invalid method call")
    | Field (e, i)      -> (match eval classes e with
                           | New (c, cargs) -> let fs = fields classes c
                                               in get_field cargs (extract_idents fs) i
                           | Null           -> failwith "NullPointerException"
                           | _              -> failwith "Unable to reduce expression - invalid field access")
    | New (i, args)     -> New (i, List.map (eval classes) args)
    | Num n             -> Num n
    | Aop (o, e1, e2)   -> (match eval classes e1 with
                           | Num n1 -> (match eval classes e2 with
                                       | Num n2 -> eval_aop o n1 n2
                                       | _      -> failwith "Number expected")
                           | _      -> failwith "Number expected")
    | UnaryMinus e      -> (match eval classes e with
                           | Num n -> Num (-n)
                           | _     -> failwith "Number expected")
    | Cop (o, e1, e2)   -> (match eval classes e1 with
                           | Num n1 -> (match eval classes e2 with
                                       | Num n2 -> eval_cop o n1 n2
                                       | _      -> failwith "Number expected")
                           | _      -> failwith "Number expected")
    | OCop (o, e1, e2)  -> let v1 = eval classes e1
                           in let v2 = eval classes e2
                           in eval_ocop o v1 v2
    | BConst c          -> BConst c
    | Bop (o, e1, e2)   -> (match eval classes e1 with
                           | BConst c1 -> (match eval classes e2 with
                                          | BConst c2 -> eval_bop o c1 c2
                                          | _         -> failwith "Boolean expected")
                           | _         -> failwith "Boolean expected")
    | Not e             -> (match eval classes e with
                           | BConst c -> BConst (match c with
                                                | JTrue  -> JFalse
                                                | JFalse -> JTrue)
                           | _        -> failwith "Boolean expected")
    | Null              -> Null
    | Cast (t, e)       -> eval_cast classes t (eval classes e)
    | If (e, e1, e2)    -> (match eval classes e with
                           | BConst JTrue  -> eval classes e1
                           | BConst JFalse -> eval classes e2
                           | _             -> failwith "Boolean expected")
  with exc -> match exc with
              | Failure s      -> print_endline s; print_string "In: "; pretty_printer_expr e; print_endline ""; raise ReductionError
              | ReductionError -> print_string "In: "; pretty_printer_expr e; print_endline ""; raise ReductionError
              | _              -> print_endline "Unknown error"; print_string "In: "; pretty_printer_expr e; print_endline ""; raise ReductionError

