val split_on : (char -> bool) -> string -> string list
val split : string -> string list
val join : string -> string list -> string
val startswith : string -> string -> bool
val range : int -> int list
val bin_search : 'a array -> 'a -> int
val time_it : ('a -> 'b) -> 'a -> float * 'b
val read_line : unit -> string option
val random_int : int -> int
val seq_search : 'a array -> 'a -> int
val log2 : int -> int
val odd: int -> bool
val even: int -> bool
val exp2: int -> int
