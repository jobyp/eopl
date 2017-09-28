
type sym = Sym of string

type exp = Const of int
         | Diff of exp * exp
         | Zero of exp
         | If of exp * exp * exp
         | Var of sym
         | Let of exp * exp * exp
         | Proc of exp * exp
         | Call of exp * exp
         | Letrec of exp * exp * exp * exp
                                         
type program = Program of exp

type exp_val = Num_val of int
             | Bool_val of bool
             | Proc_val of sym * exp * env
 and env     = Empty
             | Extend of sym * exp_val * env
             | Extend_rec of sym * sym * exp * env

let symbol_equal s1 s2 =
  match s1, s2 with
  | Sym s1', Sym s2' -> s1' = s2'


let string_of_symbol s =
  match s with
  | Sym s' -> s'

let empty_env () = Empty

let extend_env env var value =
  Extend (var, value, env)

let extend_env_rec env proc var value =
  Extend_rec (proc, var, value, env)
         
let rec apply_env env var =
  match env with
  | Empty                   -> raise (Failure ("no binding for " ^ string_of_symbol var))
  | Extend (s, v, e)        -> if symbol_equal s var then v
                               else apply_env e var
  | Extend_rec (p, s, v, e) -> if symbol_equal var p then Proc_val (s, v, env)
                               else apply_env e var

let rec value_of exp env =
  match exp with
  | Const n -> Num_val n
  | Var v -> apply_env env v
  | Diff (e1, e2) -> ( match value_of e1 env, value_of e2 env with
                       | Num_val n1, Num_val n2 -> Num_val (n1 - n2)
                       | _, _                   -> raise (Failure "Type error 1"))
  | Zero e -> let n = value_of e env in
              ( match n with
                | Num_val n -> Bool_val (n = 0)
                | _         -> raise (Failure "Type error 2"))
  | If (e1, e2, e3) -> ( match value_of e1 env with
                         | Bool_val true -> value_of e2 env
                         | Bool_val false -> value_of e3 env
                         | _ -> raise (Failure "Type error 3"))
  | Let (e1, e2, e3) -> ( match e1 with
                          | Var s -> let exp_val = value_of e2 env in
                                     let env' = extend_env env s exp_val in
                                     value_of e3 env'
                          | _ -> raise (Failure "Expected Var exp 4"))
  | Proc (v, e) -> ( match v with
                     | Var s -> Proc_val (s, e, env)
                     | _     -> raise (Failure "Expected Var exp 5"))
  | Call (e1, e2) -> ( match (value_of e1 env) with
                       | Proc_val (s, e1', env') -> let v = value_of e2 env in
                                                    let env'' = extend_env env' s v in
                                                    value_of e1' env''
                       | _ -> raise (Failure "Expected operator exp 6"))
  | Letrec (p, v, body, e) -> ( match p, v with
                                | Var p', Var v' -> value_of e (extend_env_rec env p' v' body)
                                | _              -> raise (Failure "Expected proc_name and variable 7"))
                                


let value_of_program program =
  match program with
  | Program exp -> value_of exp Empty


let () =
  Printf.printf "Tests are missing!\n";
  exit 0

