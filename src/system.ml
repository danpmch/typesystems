
open Batteries
open Grammar

let rec isnumerical : term -> bool = function
   | TmZero(_) -> true
   | TmSucc(_, t1) -> isnumerical t1
   | _ -> false

let rec isval : term -> bool = function
   | TmTrue(_) -> true
   | TmFalse(_) -> true
   | t when isnumerical t -> true
   | _ -> false

exception NoRuleApplies

let dummyinfo = ()

let rec eval1 : term -> term = function
   | TmIf(_, TmTrue(_), t2, _) -> t2
   | TmIf(_, TmFalse(_), _, t2) -> t2
   | TmIf(fi, t1, t2, t3) ->
         let t1' = eval1 t1 in
         TmIf(fi, t1', t2, t3)
   | TmSucc(fi, t1) ->
         let t1' = eval1 t1 in
         TmSucc(fi, t1')
   | TmPred(_, TmZero(_)) ->
         TmZero(dummyinfo)
   | TmPred(_, TmSucc(_, nv1)) when isnumerical nv1 ->
         nv1
   | TmPred(fi, t1) ->
         let t1' = eval1 t1 in
         TmPred(fi, t1')
   | TmIsZero(_, TmZero(_)) ->
         TmTrue(dummyinfo)
   | TmIsZero(_, TmSucc(_, nv1)) when isnumerical nv1 ->
         TmFalse(dummyinfo)
   | TmIsZero(fi, t1) ->
         let t1' = eval1 t1 in
         TmIsZero(fi, t1')
   | _ ->
         raise NoRuleApplies

let rec eval (t : term) : term =
   try let t' = eval1 t
       in eval t'
   with NoRuleApplies -> t

let parse_file (file : BatIO.input) : term option =
  Parser.expr Lexer.read (Lexing.from_channel file)

let () =
   let filename = try Sys.argv.(1)
      with _ -> print_endline "You must specify a file to evaluate"; exit (-1) in
   let ast = try File.with_file_in filename parse_file
      with Sys_error(_) -> print_endline ("Could not open file: " ^ filename); exit(-1) in
   Option.may (fun r ->
      let original_text = to_string r in
      print_endline original_text;
      print_endline (to_string (eval r)))
      ast

