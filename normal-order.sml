structure NormalOrder : SMALL_STEP = struct

  structure U = ULC
  structure S = Subst

  fun step (U.App (U.Abs (x, t1), t2)) = 
    (* E-AppAbs *)
    SOME (S.subst x t2 t1)
    | step (U.App (na, t2)) =
    (* E-App1 and E-App2 *)
    (case na
       of (U.App _) => 
       (case step na
          of (SOME na') => SOME (U.App (na', t2))
           | NONE => 
           (case step t2
              of (SOME t2') => SOME (U.App (na, t2'))
              | NONE => NONE))
        | _ =>
        (case step t2
           of (SOME t2') => SOME (U.App (na, t2'))
            | NONE => NONE))
    | step (U.Abs (x, t1)) =
    (* E-Abs *)
    (case step t1
      of NONE => NONE
        | SOME t1' => SOME (U.Abs (x, t1')))
    | step (U.Var _) = NONE
				 	       
end
