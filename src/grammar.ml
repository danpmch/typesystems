
type info = unit

type term =
     TmTrue of info
   | TmFalse of info
   | TmIf of info * term * term * term
   | TmZero of info
   | TmSucc of info * term
   | TmPred of info * term
   | TmIsZero of info * term

let rec to_string = function
   | TmTrue(_) -> "true"
   | TmFalse(_) -> "false"
   | TmIf(_, t1, t2, t3) ->
         let s1 = to_string t1
         and s2 = to_string t2
         and s3 = to_string t3 in
         "if (" ^ s1 ^ ") (" ^ s2 ^ ") (" ^ s3 ^ ")"
   | TmZero(_) -> "z"
   | TmSucc(_, t1) ->
         "succ (" ^ (to_string t1) ^ ")"
   | TmPred(_, t1) ->
         "pred (" ^ (to_string t1) ^ ")"
   | TmIsZero(_, t1) ->
         "zero? (" ^ (to_string t1) ^ ")"

