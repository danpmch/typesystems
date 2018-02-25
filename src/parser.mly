
%token TRUE
%token FALSE
%token IF
%token ZERO
%token SUCC
%token PRED
%token IS_ZERO
%token EOF

%start <Grammar.term option> expr
%%

expr:
   | EOF       { None }
   | t = term  { Some t }
   ;

term:
   | TRUE      { Grammar.TmTrue () }
   | FALSE     { Grammar.TmFalse () }
   | IF; t1 = term; t2 = term; t3 = term    { Grammar.TmIf((), t1, t2, t3) }
   | ZERO      { Grammar.TmZero () }
   | SUCC; t = term  { Grammar.TmSucc( (), t ) }
   | PRED; t = term  { Grammar.TmPred( (), t ) }
   | IS_ZERO; t = term { Grammar.TmIsZero( (), t ) }
   ;



