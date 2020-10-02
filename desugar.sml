structure Desugar : sig

  val desugar : SULC.term -> ULC.term

end = struct

  structure S = SULC
  structure U = ULC

  (* churchnum : int -> ULC.term *)
  fun churchnum nv = U.Abs ("s", U.Abs ("z", cnum nv))
  and cnum 0 = U.Var "z"
    | cnum nv = U.App (U.Var "s", cnum (nv-1))

  val tru = U.Abs ("a", U.Abs ("b", U.Var "a"))
  val fls = U.Abs ("a", U.Abs ("b", U.Var "b"))
  val fst = U.App (U.Abs ("p", U.Var "p"), tru)
  val snd = U.App (U.Abs ("p", U.Var "p"), fls)

  fun desugar (S.Var x) = U.Var x
    | desugar (S.Abs (x, t)) = U.Abs (x, desugar t)
    | desugar (S.App (t1, t2)) = U.App (desugar t1, desugar t2)
    | desugar (S.Nat nv) = churchnum nv
    | desugar (S.ID x) = U.Abs (x, U.Var x)
    | desugar (S.Pair (t1, t2)) = U.Abs ("b", U.App (U.App (U.Var "b", desugar t1), desugar t2))
    | desugar (S.Select1 t) = U.App (fst, desugar t)
    | desugar (S.Select2 t) = U.App (snd, desugar t)
      
end
