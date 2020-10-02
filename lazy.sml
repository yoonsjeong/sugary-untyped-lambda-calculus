structure Lazy : SMALL_STEP = struct

  structure U = ULC
  structure S = Subst

  fun step (U.App (U.Abs (x, t1), t2)) =
  (* E-AppAbs *)
    SOME (S.subst x t2 t1)
    | step (U.App (t1, t2)) =
  (* E-App1 *)
    (case step t1
      of SOME t1' => SOME (U.App (t1', t2))
       | NONE => NONE)
    | step _ = NONE

end
