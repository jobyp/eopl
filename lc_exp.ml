
type sym = Sym of string

type lc_exp = Id of sym
            | Lambda of sym * lc_exp
            | Lc_appl of lc_exp * lc_exp


let symbol_equal s1 s2 =
  match s1, s2 with
  | Sym s1', Sym s2' -> s1' = s2'


let rec occurs_free sym exp =
  match exp with
  | Id s                  -> symbol_equal s sym
  | Lambda (s, lexp)      -> not (symbol_equal s sym)
                             && occurs_free sym lexp
  | Lc_appl (lexp, lexp') -> occurs_free sym lexp
                             || occurs_free sym lexp'


let test_01 () =
  let x = Sym "x" in
  let id_x = Id x in
  let y = Sym "y" in
  let id_y = Id y in
  let z = Sym "z" in
  let id_z = Id z in
  assert (occurs_free x id_x);
  assert (not (occurs_free x id_y));
  assert (not (occurs_free x (Lambda (x, Lc_appl (id_x, id_y)))));
  assert (occurs_free x (Lambda (y, Lc_appl (id_x, id_y))));
  assert (occurs_free x
                      (Lc_appl
                         ((Lambda
                             (x, Lc_appl (id_x, id_y))),
                          (Lc_appl (id_x, id_y)))));
  assert (occurs_free x
                      (Lambda (y,
                               (Lambda (z,
                                        (Lc_appl (id_x,
                                                  Lc_appl (id_y, id_z))))))))



let () =
  test_01 ()

