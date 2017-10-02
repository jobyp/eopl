(* S-expression Grammar *)

type symbol = Sym of string
            | Num of int
                     
type sexp = Nil
          | Atom of symbol
          | Cons of sexp * sexp
           

let rec find_token_end str j =
  if j >= String.length str then j
  else
    match str.[j] with
    | '('                      -> j
    | ')'                      -> j
    | ' ' | '\n' | '\r' | '\t' -> j
    | _                        -> find_token_end str (j + 1)


let isspace c =
  match c with
  | ' ' | '\n' | '\r' | '\t' -> true
  | _                        -> false

                                  
let rec get_next_token str i =
  if i >= String.length str then None
  else if (isspace str.[i]) then get_next_token str (i + 1) (* Skip white spaces *)
  else if str.[i] = '(' then Some ("(", (i + 1))
  else if str.[i] = ')' then Some (")", (i + 1))
  else 
    let j = find_token_end str (i + 1) in
    Some (String.sub str i (j - i), j)

         
let rec scanner_aux str i =
  match get_next_token str i with
  | None -> []
  | Some (token, j) -> token :: scanner_aux str j


let scanner str =
  scanner_aux str 0
              

let () =
  Printf.printf "Hello, World!\n"
