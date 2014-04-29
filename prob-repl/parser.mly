%{
  open ExtList
  open ExtHashtbl
%}

%token TEOF

%token <string> STRING
%token <int> INT
%token <string> IDENT
%token <string> LABEL
%token IF PIF THEN ELSE ENDIF ENDPIF WHILE DO ENDWHILE
%token LB RB LP RP LSB RSB RA
%token SEMICOLON COLON DOT COMMA
%token LNEG LEQ GEQ LESSER GREATER EQUAL NEQUAL

%token ASSERT OBSERVE
%token TRUE FALSE

%token PLUS MINUS
%token TIMES DIVIDES
%token LAND LOR

%token AAND AOR AXOR ANEG

%token SKIP EQUALS

%token DEFINE INCLUDE UI
%token UIRESET UIVIEW UIEXIT

%token UNIFORM RAND RANDINT RANDREAL RANDBOOL

%token FUN RETURN TO
%token TYPE TUNIT TBOOL TINT TREAL TSTRING TRECORD

%right SEMICOLON
%nonassoc EQUALS
%left LOR LAND AOR AAND AXOR
%left LEQ GEQ LESSER GREATER EQUAL NEQUAL
%left PLUS MINUS
%left TIMES DIVIDES

%left LP
%left LSB  
%left DOT
%left RA

%nonassoc NONASSOC

%type <Lang.stmt> stmt
%type <Lang.datatype> datatype
%type <Lang.stmt> stmt_eof
%type <Lang.pstmt> pstmt_eof

%start stmt_eof
%start pstmt_eof

%token <float> FLOAT

%%

real :
 | FLOAT { $1 }
;

unop1 :
 | MINUS { "-" }
;

unop2 :
 | LNEG { "not" }
 | ANEG { "!" }
;
 
binop1 :
 | TIMES   { "*" }
 | DIVIDES { "/" }
;

binop2 :
 | PLUS    { "+" }
 | MINUS   { "-" }
;

binop3 :
 | LEQ     { "<=" }
 | GEQ     { ">=" }
 | LESSER  { "<" }
 | GREATER { ">"  }
 | EQUAL   { "==" }
 | EQUALS  { "=" }
 | NEQUAL  { "!=" }
;

binop4 :
 | LAND    { "and" }
 | LOR     { "or" }
 | AAND    { "&" }
 | AOR     { "|" }
 | AXOR    { "^" }
;

ident_list :
 | IDENT { [$1] }
 | IDENT COMMA ident_list { $1 :: $3 }
;

value :
 | LP RP { Lang.VUnit }
 | TRUE  { Lang.VBool (true) }
 | FALSE { Lang.VBool (false) }
 | INT   { Lang.VInt  ($1) }
 | real  { Lang.VReal ($1) }
 | STRING { Lang.VString ($1) }
;

lvalue :
 | IDENT { Lang.EIdent ($1) }
 | lvalue DOT IDENT %prec DOT { Lang.EMember ($1, $3) }
;

namedexp :
 | IDENT EQUALS exp { ($1, $3) }
;

namedexp_list :
 | namedexp { [$1] }
 | namedexp SEMICOLON namedexp_list { $1 :: $3 }
;

randkeyword :
 | RAND { }
 | UNIFORM { }
;

exp :
 | IDENT { Lang.EIdent ($1) }
 | exp DOT IDENT %prec DOT { Lang.EMember ($1, $3) }
 | LB namedexp_list RB { Lang.ERecord $2 }
 | value { Lang.EValue ($1) }
 | exp binop1 exp %prec TIMES { Lang.EBinop ($2, $1, $3) }
 | exp binop2 exp %prec PLUS  { Lang.EBinop ($2, $1, $3) }
 | exp binop3 exp %prec LEQ   { Lang.EBinop ($2, $1, $3) }
 | exp binop4 exp %prec LAND  { Lang.EBinop ($2, $1, $3) }
 | unop1 exp %prec NONASSOC { Lang.EUnop ($1, $2) }
 | unop2 exp %prec NONASSOC { Lang.EUnop ($1, $2) }
 | IDENT LP explist RP { Lang.EApp ($1, $3) }
 | LP exp RP { $2 }
 | RANDINT INT { Lang.ERandInt (0, $2) }
 | RANDREAL real { Lang.ERandReal (0.0, $2) }
 | RANDBOOL real { Lang.ERandBool $2 }
 | randkeyword real { Lang.ERandReal (0.0, $2) }
 | randkeyword INT { Lang.ERandInt (0, $2) }
 | randkeyword real real { Lang.ERandReal ($2, $3) }
 | randkeyword INT INT { Lang.ERandInt ($2, $3) }
;

explist :
 | { [] }
 | exp { [$1] }
 | exp COMMA explist %prec COMMA { $1 :: $3 }
;

namedtype_pair :
 | IDENT COLON datatype { ($1, $3) }
;

namedtype_list :
 | namedtype_pair { [$1] }
 | namedtype_pair SEMICOLON namedtype_list %prec SEMICOLON { $1 :: $3 }
;

/*
datatype_list :
 | datatype { [$1] }
 | datatype COMMA datatype_list { $1 :: $3 }
;
*/

datatype :
 | TUNIT { Lang.TUnit }
 | TBOOL { Lang.TBool }
 | TINT { Lang.TInt }
 | TREAL { Lang.TReal }
 | TSTRING { Lang.TString }
 | LB namedtype_list RB { Lang.TRecord $2 }
 | IDENT { Lang.TIdent ($1) }
/* | LP RP RA datatype { Lang.TFun (TUnit, $4) }
 | LP datatype_list RP RA datatype %prec RA { Lang.TFun ($2, $5) }*/
;


stmt :
 | stmtcompound { $1 }
 | stmtbasic { $1 }
 | stmtcontrol { $1 }
 | stmtdefine { $1 }
;

stmtcompound :
 | LB RB { Lang.SSkip }
 | LB stmtlist RB { Lang.seq_of_list $2 }
;

funbinds_list :
 | { [] }
 | datatype IDENT { [($2, $1)] }
 | datatype IDENT COMMA funbinds_list { ($2, $1) :: $4}
;

stmtdefine :
 | datatype ident_list SEMICOLON
     { Lang.seq_of_list
       (List.map
          (fun id -> Lang.SDefineValue (id, $1))
          $2)
     }
 | datatype IDENT EQUALS exp SEMICOLON
     { Lang.SSeq(Lang.SDefineValue ($2, $1),
                 Lang.SAssign (Lang.EIdent $2, $4)) }
 | datatype IDENT LP funbinds_list RP stmt
     { Lang.SDefineFun ($2, $1, $4, $6) } 
;

stmtbasic :
 | SKIP SEMICOLON
     { Lang.SSkip }
 | lvalue EQUALS exp SEMICOLON
     { Lang.SAssign ($1, $3) }
 | RETURN exp SEMICOLON
     { Lang.SReturn $2 }
 | TYPE IDENT EQUALS datatype SEMICOLON
     { Lang.SDefineType ($2, $4) }
 | ASSERT exp SEMICOLON
     { Lang.SAssert ($2) }
 | OBSERVE exp SEMICOLON
     { Lang.SAssert ($2) }
;

stmtcontrol :
/* parsing conflic due to the two if rules */
 | IF LP exp RP stmt 
     { Lang.SIf ($3, $5, Lang.SSkip) }
 | IF LP exp RP stmt ELSE stmt
     { Lang.SIf ($3, $5, $7) }
 | WHILE LP exp RP stmt
     { Lang.SWhile ($3, Lang.EValue (Lang.VBool true), $5) }
 | WHILE LP exp RP 
     LP exp RP stmt
     { Lang.SWhile ($3, $6, $8) }
;

stmtlist :
 | stmt { [$1] }
 | stmt stmtlist { $1 :: $2 }
;

stmt_eof :
 | stmtlist TEOF { Lang.seq_of_list $1 }
;

uicmd :
 | UIRESET INT { Lang.UiReset ($2, 2) }
 | UIRESET INT INT { Lang.UiReset ($2, $3) }
 | UIVIEW lvalue lvalue lvalue { Lang.UiView ($2, $3, $4) }
 | UIEXIT { Lang.UiExit }
;

pstmt :
 | INCLUDE STRING pstmt { Lang.PSInclude ($2, $3) }
 | INCLUDE STRING { Lang.PSInclude ($2, Lang.PSStmt Lang.SSkip) }
 | DEFINE IDENT exp pstmt { Lang.PSDefineExp ($2, $3, $4) }
 | stmtlist { Lang.PSStmt ( Lang.seq_of_list $1 ) }
 | uicmd pstmt { Lang.PSSeq (Lang.PSUi $1, $2) }
 | uicmd { Lang.PSSeq (Lang.PSUi $1, Lang.PSStmt Lang.SSkip) }

;
pstmt_eof :
 | pstmt TEOF { $1 }
;

%%
