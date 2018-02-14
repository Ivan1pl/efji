open Syntax

type mType = jType list * jType
type mBody = ident list * jExpr

let rec get_class classes c =
  match classes with
  | []                                  -> None
  | Class (i1, i2, _, _, _) as cl :: cs -> if i1 = c then Some cl else get_class cs c

let rec is_subtype classes c d =
  match c with
  | Int     -> c = d
  | Boolean -> c = d
  | Named c -> match d with
               | Int     -> false
               | Boolean -> false
               | Named d -> if c = d || c.id = ""
                            then true
                            else match get_class classes c with
                                 | Some (Class (i1, i2, _, _, _)) -> is_subtype classes (Named i2) (Named d)
                                 | None                           -> if c.id = "Object" then d.id = "Object" else failwith ("Class " ^ c.id ^ " not found")

let extract_jtypes vs =
  List.map (function Var (t, _) -> t) vs

let extract_idents vs =
  List.map (function Var (_, i) -> i) vs

let mtype classes m c =
  let rec mtype_l ms =
    match ms with
    | []                         -> None
    | Method (t, i, vs, _) :: ms -> if m.id = i.id
                                    then Some (extract_jtypes vs, t)
                                    else mtype_l ms
  in let rec mtype_internal cs c =
    match cs with
    | []                             -> failwith ("Method " ^ m.id ^ " not found")
    | Class (i1, i2, _, _, ms) :: cs -> if i1.id = c.id
                                        then match mtype_l ms with
                                             | Some t -> t
                                             | None   -> mtype_internal classes i2
                                        else mtype_internal cs c
  in mtype_internal classes c

let mbody classes m c =
  let rec mbody_l ms =
    match ms with
    | []                         -> None
    | Method (_, i, vs, e) :: ms -> if m.id = i.id
                                    then Some (extract_idents vs, e)
                                    else mbody_l ms
  in let rec mbody_internal cs c =
    match cs with
    | []                             -> failwith ("Method " ^ m.id ^ " not found")
    | Class (i1, i2, _, _, ms) :: cs -> if i1.id = c.id
                                        then match mbody_l ms with
                                             | Some b -> b
                                             | None   -> mbody_internal classes i2
                                        else mbody_internal cs c
  in mbody_internal classes c

let fields classes c =
  let rec fields_internal cs c =
    if c.id = "Object"
    then []
    else match cs with
         | []                             -> failwith ("Class " ^ c.id ^ " not found")
         | Class (i1, i2, fs, _, _) :: cs -> if i1.id = c.id
                                             then (fields_internal classes i2) @ fs
                                             else fields_internal cs c
  in fields_internal classes c

exception TypingError

type env = (ident * jType) list

let empty_env = []
let extend_env vars types e = (List.combine vars types) @ e

let rec apply_env env v =
  match env with
  | []            -> failwith ("Undefined variable: " ^ v.id)
  | (i, t) :: env -> if i = v then t else apply_env env v

let rec jExpr_type classes e env =
  let rec is_all_subtype_err cs l1 l2 =
    match l1 with
    | []      -> (match l2 with
                 | [] -> ()
                 | _  -> failwith "Argument count mismatch")
    | t :: ts -> match l2 with
                 | []        -> failwith "Argument count mismatch"
                 | t2 :: ts2 -> if is_subtype classes t t2
                                then is_all_subtype_err cs ts ts2
                                else failwith "Invalid argument"
  in let rec get_field_type fields id =
    match fields with
    | []               -> failwith ("Field " ^ id.id ^ " not found")
    | Var (t, i) :: fs -> if i = id then t else get_field_type fs id
  in try
    match e with
    | This              -> apply_env env (ident "this")
    | Val i             -> apply_env env i
    | Call (e, i, args) -> let te = jExpr_type classes e env
                           in (match te with
                              | Int | Boolean -> failwith "Method call on value of primitive type"
                              | Named t       -> let (ats, rt) = mtype classes i t
                                                 in let argtypes = List.map (fun expr -> jExpr_type classes expr env) args
                                                 in is_all_subtype_err classes argtypes ats; rt)
    | Field (e, i)      -> let te = jExpr_type classes e env
                           in (match te with
                              | Int | Boolean -> failwith "Field access on value of primitive type"
                              | Named t       -> let fs = fields classes t
                                                 in get_field_type fs i)
    | New (i, args)     -> let fs = fields classes i
                           in let ats = extract_jtypes fs
                           in let argtypes = List.map (fun expr -> jExpr_type classes expr env) args
                           in is_all_subtype_err classes argtypes ats; Named i
    | Num _             -> Int
    | Aop (_, e1, e2)   -> (match jExpr_type classes e1 env with
                           | Int -> (match jExpr_type classes e2 env with
                                    | Int -> Int
                                    | _   -> failwith "Expected int")
                           | _   -> failwith "Expected int")
    | UnaryMinus e      -> (match jExpr_type classes e env with
                           | Int -> Int
                           | _   -> failwith "Expected int")
    | Cop (_, e1, e2)   -> (match jExpr_type classes e1 env with
                           | Int -> (match jExpr_type classes e2 env with
                                    | Int -> Boolean
                                    | _   -> failwith "Expected int")
                           | _   -> failwith "Expected int")
    | OCop (_, e1, e2)  -> let t1 = jExpr_type classes e1 env
                           in let t2 = jExpr_type classes e2 env
                           in if is_subtype classes t1 t2 || is_subtype classes t2 t1 then Boolean else failwith "Incompatible types"
    | BConst _          -> Boolean
    | Bop (_, e1, e2)   -> (match jExpr_type classes e1 env with
                           | Boolean -> (match jExpr_type classes e2 env with
                                        | Boolean -> Boolean
                                        | _       -> failwith "Expected boolean")
                           | _       -> failwith "Expected boolean")
    | Not e             -> (match jExpr_type classes e env with
                           | Boolean -> Boolean
                           | _       -> failwith "Expected boolean")
    | Null              -> Named (ident "")
    | Cast (t, e)       -> let te = jExpr_type classes e env
                           in (match t with
                              | Int     -> (match te with
                                           | Int     -> Int
                                           | Boolean -> Int
                                           | Named _ -> failwith "Invalid cast from object type to int")
                              | Boolean -> (match te with
                                           | Int     -> Boolean
                                           | Boolean -> Boolean
                                           | Named _ -> failwith "Invalid cast from object type to boolean")
                              | Named c -> (match te with
                                           | Int     -> failwith "Invalid cast from int to object type"
                                           | Boolean -> failwith "Invalid cast from boolean to object type"
                                           | Named d -> if is_subtype classes t te || is_subtype classes te t
                                                        then t
                                                        else failwith ("Invalid cast from type " ^ d.id ^ " to type " ^ c.id)))
    | If (e, e1, e2)    -> let te = jExpr_type classes e env
                           in let t1 = jExpr_type classes e1 env
                           in let t2 = jExpr_type classes e2 env
                           in (match te with
                              | Boolean -> if is_subtype classes t1 t2
                                           then t2
                                           else if is_subtype classes t2 t1
                                                then t1
                                                else (match t1 with
                                                     | Named _ -> (match t2 with
                                                                  | Named _ -> Named (ident "Object")
                                                                  | _       -> failwith "If branches have incompatible types")
                                                     | _       -> failwith "If branches have incompatible types")
                              | _       -> failwith "Boolean expected in if condition")
  with exc -> match exc with
              | Failure s   -> print_endline s; print_string "In: "; pretty_printer_expr e; print_endline ""; raise TypingError
              | TypingError -> print_string "In: "; pretty_printer_expr e; print_endline ""; raise TypingError
              | _           -> print_endline "Unknown error"; print_string "In: "; pretty_printer_expr e; print_endline ""; raise TypingError

let method_ok classes m c =
  match m with
  | Method (c0, m, x, e0) -> let te0 = jExpr_type classes e0 (extend_env ((extract_idents x) @ [ident "this"]) ((extract_jtypes x) @ [Named c]) empty_env)
                             in let d = (match get_class classes c with
                                        | Some (Class (_, i2, _, _, _)) -> i2
                                        | None                          -> failwith ("Invalid class: " ^ c.id))
                             in if is_subtype classes te0 c0
                                then let (ats, rt) = try mtype classes m d
                                                     with exc -> (extract_jtypes x, c0)
                                     in if ats = (extract_jtypes x) && rt = c0 then () else failwith "Invalid override"
                                else failwith "Expression type does not match the return type of the method"

let class_ok classes c =
  let rec get_and_remove_class cl c =
    match cl with
    | []                          -> failwith "Inheritance loop detected"
    | Class (i, _, _, _, _) as x :: cl -> if i = c
                                          then (x, cl)
                                          else let (y, xs) = get_and_remove_class cl c
                                               in (y, x :: xs)
  in let rec detect_loop c classes =
    if c.id = "Object"
    then ()
    else let (Class (c, d, _, _, _), cl) = get_and_remove_class classes c
         in detect_loop d cl
  in match c with
  | Class (c, d, fs, k, ms) -> detect_loop c classes;
                               let gs = fields classes d
                               in (match k with
                                  | Constructor (n, args, gss, fss) -> if c = n
                                                                       then if args = gs @ fs
                                                                            then if extract_idents gs = gss && extract_idents fs = fss
                                                                                 then List.iter (fun m -> method_ok classes m c) ms
                                                                                 else failwith "Invalid constructor body"
                                                                            else failwith "Invalid arguments for constructor"
                                                                       else failwith "Constructor name does not match class name")

