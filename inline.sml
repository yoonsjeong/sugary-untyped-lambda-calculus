structure Inline : sig

  val inline : SULC.program -> SULC.term

end = struct

  structure S = SULC

  fun member (nil, x') = false
    | member ((x, _)::t, x') = if (x = x') then true else member (t, x')

  fun lookup (nil, t) = raise Fail ("variable "^t^" not found") 
    | lookup ((x, abbr)::t, x') =
      if (x = x') then abbr else lookup (t, x')

  fun scope (dict, S.Abbr (x, abbr, prog)) = scope (addvar (x, dict, abbr), prog)
    | scope (dict, S.Term t) = lp (dict, t)
  and lp (dict, S.Var x) = 
    (case explode x
       of (#"_"::t) => lookup (dict, x)
        | _ => S.Var x)
    | lp (dict, S.Abs (x, t)) = S.Abs (x, lp (dict, t))
    | lp (dict, S.App (t1, t2)) = S.App (lp (dict, t1), lp (dict, t2))
    | lp (dict, S.Nat nv) = S.Nat nv
    | lp (dict, S.ID x) = S.ID x
    | lp (dict, S.Pair (t1, t2)) = S.Pair (lp (dict, t1), lp (dict, t2))
    | lp (dict, S.Select1 t) = S.Select1 (lp (dict, t))
    | lp (dict, S.Select2 t) = S.Select2 (lp (dict, t))
  and addvar (x, dict, abbr) = 
    if member (dict, x) 
    then raise Fail "attempted redefine existing variable"
    else ((x, lp (dict, abbr))::dict)

  fun inline prog = scope (nil, prog) 

end
