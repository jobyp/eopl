type variable = string
type fname = string
type arg = string

type expression = 
  | Cons_exp of int (* OCaml int *)
  | Diff_exp of (expression * expression)
  | Zero_exp of expression
  | If_exp of (expression * expression * expression)
  | Var_exp of variable
  | Let_exp of (variable * expression * expression)
  | Proc_exp of (variable * expression)
  | Letrec_exp of (fname * arg * expression * expression)
  | Call_exp of (expression * expression)

type program = A_program of expression

type body = expression

type env = 
  | Empty_env
  | Extend_env of (variable * value * env)
  | Extend_env_rec of (fname * arg * body * env)
and value = 
  | Int of int
  | Bool of bool
  | Proc of (variable * expression * env)


let empty_env () = Empty_env

let extend_env variable value env = Extend_env (variable, value, env)

let extend_env_rec fname arg body env = Extend_env_rec (fname, arg, body, env)

let rec apply_env variable = function 
  | Empty_env -> failwith ("apply_env: binding not found for " ^ variable)
  | Extend_env (var, val', _) when var = variable -> val'
  | Extend_env (_, _, env) -> apply_env variable env
  | Extend_env_rec (fname, arg, body, saved_env) as env ->
      if fname = variable
      then Proc (arg, body, env)
      else apply_env variable saved_env

let init_env = (* i = 1; v = 5; x = 10 *)
  extend_env "i" (Int 1) 
    (extend_env "v" (Int 5)
       (extend_env "x" (Int 10) 
          (empty_env ())))

let minus v1 v2 = 
  match v1, v2 with
    | Int i1, Int i2 -> Int (i1 - i2)
    | _, _ -> raise (Invalid_argument "minus: expected Ints")

let is_zero = function
  | Int n when n = 0 -> true
  | _ -> false

let rec value_of_exp exp env = 
  match exp with
    | Cons_exp i -> Int i
    | Var_exp var -> apply_env var env
    | Diff_exp (exp1, exp2) -> let v1 = value_of_exp exp1 env in
                            let v2 = value_of_exp exp2 env in
                            minus v1 v2
    | Zero_exp exp -> Bool (is_zero (value_of_exp exp env))
    | If_exp (exp1, exp2, exp3) -> 
        (match (value_of_exp exp1 env) with
          | Bool true -> value_of_exp exp2 env
          | Bool false -> value_of_exp exp3 env
          | _ -> raise (Invalid_argument "value_of_exp: expected a boolean expression"))
    | Let_exp (var, exp1, exp2) ->
        let new_env = extend_env var (value_of_exp exp1 env) env in
        value_of_exp exp2 new_env
    | Proc_exp (var, exp) -> Proc (var, exp, env)
    | Letrec_exp (fname, arg, body, exp) -> 
        let new_env = extend_env_rec fname arg body env in
        value_of_exp exp new_env
    | Call_exp (rator, rand) ->
        (match (value_of_exp rator env) with
          | Proc (var, exp, saved_env) -> 
              let let_exp = Let_exp (var, rand, exp) in
              value_of_exp let_exp saved_env
          | _ -> raise (Invalid_argument "value_of_exp: expected a procedure"))

let value_of_program = function
  | A_program exp -> value_of_exp exp init_env
