(* utility functions *)

let _ = Random.self_init ()

let is_whitespace = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false

let split_on fn s =
  let s_len = String.length s in
  let rec sub_string lst i j =
    if j == s_len then String.sub s i (j - i) :: lst
    else if fn (s.[j]) then sub_string (String.sub s i (j - i) :: lst) (j + 1) (j + 1)
    else sub_string lst i (j + 1)
  in
  List.rev (sub_string [] 0 0)

(* Same as Python's split *)
let split s = List.filter (function s -> s <> "") (split_on is_whitespace s)

let strip s =
  let s_len = String.length s in
  let rec index_l i =
    if i = s_len then (i - 1) 
    else if is_whitespace (s.[i]) then index_l (i + 1)
    else i in
  let rec index_r i j = 
    if (j = i) then j
    else if j <= 0 then 0
    else if is_whitespace (s.[j]) then index_r i (j - 1)
    else j + 1 in
  let i = index_l 0 in
  let j = index_r i (s_len - 1) in
  if s = "" then "" else String.sub s i (j - i)
  
let rec join s = function
  | [] -> ""
  | (w::ws) -> (w ^ s) ^ join s ws

let startswith s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let rec check i = 
    if (i >= l2) then true (* bound check *)
    else if s1.[i] = s2.[i] then check (i + 1) 
    else false
  in
  if l1 < l2 then false
  else check 0

let range n = 
  let rec range_aux i lst = 
    if i < 0 then lst
    else range_aux (i - 1) (i :: lst)
  in range_aux (n - 1) []

let bin_search a x =
  let rec search lo hi =
    if lo > hi then -1
    else 
      let mid = lo + (hi - lo) / 2 in
      if a.(mid) < x
      then search (mid + 1) hi
      else if a.(mid) > x 
      then search lo (mid - 1)
      else mid in
  search 0 (Array.length a - 1)

let time_it action arg = 
  let t1 = Sys.time () in
  let result = action arg in
  let t2 = Sys.time () in
  (t2 -. t1, result)

let random_int n =
  match Random.bool () with
    | true -> Random.int n
    | false -> -1 * (Random.int n)
      
let rec read_line () =
  try 
    let line = String.trim (input_line stdin) in
    if line = "" then read_line () 
    else if startswith line "#" 
    then read_line ()
    else Some line
  with End_of_file -> None 

let seq_search a x =
  let n = Array.length a in
  let rec loop i = 
    if i < n 
    then
       if a.(i) = x then i else loop (i + 1) 
    else
      -1 in
  loop 0

let rec log2 n = 
  let rec lg2 n = 
    if n = 1
    then 0
    else 1 + lg2 (n / 2) in
  if n < 1
  then failwith ("log2: invalid argument " ^ (string_of_int n))
  else lg2 n

let odd n = (n mod 2 = 1)
let even n = (n mod 2 = 0)

let rec expn x n  = 
  if n <= 0
  then 1
  else if even n 
  then 
    let a = expn x (n / 2) in
    a * a
  else
    x * expn x (n - 1)

let exp2 n = expn 2 n


(* end of utility functions *)
