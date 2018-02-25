
let white = [' ' '\t' '\n']+

rule read =
   parse
   | white { read lexbuf }
   | "true" { Parser.TRUE }
   | "false" { FALSE }
   | "if" { IF }
   | "z" { ZERO }
   | "succ" { SUCC }
   | "pred" { PRED }
   | "zero?" { IS_ZERO }
   | eof { EOF }


