
type symbol = Sym of string

type value = Val of int

let string_of_symbol s =
  match s with
  | Sym s' -> s'

let symbol_equal s1 s2 =
  match s1, s2 with
  | Sym s1', Sym s2' -> s1' = s2'


let empty_env () =
  fun v -> raise (Failure ("no binding for " ^ string_of_symbol v))


let extend_env env sym value =
  fun v ->
  if symbol_equal v sym then value
  else env v


let apply_env env sym =
  env sym


let test_1 () =
  let e = empty_env () in
  let e1 = extend_env e (Sym "a") (Val 1) in
  let e2 = extend_env e1 (Sym "b") (Val 2) in
  let e3 = extend_env e2 (Sym "c") (Val 3) in
  match (apply_env e3 (Sym "a")) with
  | Val n -> assert (n = 1)


let () =
  test_1 ()
