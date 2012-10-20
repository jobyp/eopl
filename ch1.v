Inductive inS : nat -> Prop :=
| S_0 : inS 0
| S_plus_3 : forall n : nat, inS n -> inS (n + 3).

Example inS_has_6 : inS 6.
Proof.
  apply S_plus_3 with (n:=3).
  apply S_plus_3 with (n:=0).
  apply S_0.
Qed.

