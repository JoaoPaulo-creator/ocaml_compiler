open Ast

type value =
  | VInt of int
  | VUnit

type environment = (string * value) list

let rec eval_expr env = function
  | Int n -> VInt n
  | Var x ->
    (match List.assoc_opt x env with
    | Some v -> v
    | None -> failwith ("Unbound variable: " ^ x))
  
  | Binary (e1, op, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, op, v2 with
    | VInt n1, Plus, VInt n2 -> VInt (n1 + n2)
    | VInt n1, Minus, VInt n2 -> VInt (n1 - n2)
    | VInt n1, Multiply, VInt n2 -> VInt (n1 * n2)
    | VInt n1, Divide, VInt n2 -> 
      if n2 = 0 then failwith "Division by zero"
      else VInt (n1 / n2)
    | _ -> failwith "Invalid operation")
  
  | Let (x, e) ->
    let v = eval_expr env e in
    v
  | Print e -> 
    let v = eval_expr env e in
    (match v with
    | VInt n -> Printf.printf "%d\n" n
    | VUnit -> Printf.printf "()\n");
    VUnit

let eval_program prog =
  let rec eval_statements env = function
    | [] -> VUnit
    | stmt :: rest -> 
      let v = eval_expr env stmt in
      match stmt with
      | Let (x, _) -> eval_statements ((x, v) :: env) rest
      | _ -> eval_statements env rest in
      eval_statements [] prog