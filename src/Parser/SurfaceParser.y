{
module Parser.SurfaceParser where
import Parser.Lexer
import AST
}
%name parseSurface Exp
%tokentype { Token }
%error { parseError }

%token
    if { TokenIf }
    then { TokenThen }
    else { TokenElse }
    let { TokenLet }
    '=' { TokenOp "="}
    in { TokenIn }

    '->' { TokenArrow }
    lam { TokenLambda }
    '_' { TokenHole }

    '(' { TokenLParen }
    ')' { TokenRParen }
    '[' { TokenLBracket }
    ']' { TokenRBracket }
    ',' { TokenComma }

    num { TokenNum $$ }
    bool { TokenBool $$ }
    ident { TokenIdent $$ }
    builtin { TokenBuiltin $$ }

    -- fixity 9
    '.' { TokenOp "." }
    -- fixity 8
    '^' { TokenOp "^" }
    -- fixity 7
    '*' { TokenOp "*" }
    '/' { TokenOp "/" }
    '%' { TokenOp "%" }
    -- fixity 6
    '+' { TokenOp "+" }
    '-' { TokenOp "-" }
    ':+' { TokenOp ":+" }
    ':-' { TokenOp ":-" }
    -- fixity 5
    -- list operators will go here, eventually
    '#' { TokenOp "#" }
    -- fixity 4
    '==' { TokenOp "==" }
    '/=' { TokenOp "/=" }
    '!=' { TokenOp "!=" }
    '>' { TokenOp ">" }
    '<' { TokenOp "<" }
    '>=' { TokenOp ">=" }
    '<=' { TokenOp "<=" }
    '<$>' { TokenOp "<$>" }
    '<*>' { TokenOp "<*>" }
    -- fixity 3
    '&&' { TokenOp "&&" }
    -- fixity 2
    '||' { TokenOp "||" }
    -- fixity 1
    '>>=' { TokenOp ">>=" }
    '|' { TokenOp "|" }
    -- fixity 0
    '$' { TokenOp "$" }

-- fixity 0
%right '$'
-- fixity 1
%left '>>=' '|'
-- fixity 2
%right '||'
-- fixity 3
%right '&&'
-- fixity 4
%left '<$>' '<*>'
%nonassoc '==' '!=' '>' '<' '>=' '<='
-- fixity 5
-- list operators will go here, eventually
%right '#'
-- fixity 6
%left '+' '-'
%nonassoc ':+' ':-'
-- fixity 7
%left '*' '/' '%'
-- fixity 8
%right '^'
%left NEG
-- fixity 9
%right '.'

%%

Idents : ident        { [$1] }
       | ident Idents { $1 : $2 }

ListBody : Exp          { [$1]}
         | Exp ',' ListBody { $1 : $3 }

Exp   : let ident '=' Exp in Exp     { SLet (S $2) $4 $6}
      | if Exp then Exp else Exp     { SIf $2 $4 $6 }
      | lam Idents '->' Exp { foldr (\v body -> SLambda (S v) body) $4 $2 }
      -- fixity 10
      | App                          { $1 }
      -- fixity 9
      | Exp '.' Exp                  { SInfix "." $1 $3 }
      -- fixity 8
      | '-' Exp %prec NEG            { SUnaryOp "-" $2 }
      | Exp '^' Exp                  { SInfix "^" $1 $3 }
      -- fixity 7
      | Exp '*' Exp                  { SInfix "*" $1 $3 }
      | Exp '/' Exp                  { SInfix "/" $1 $3 }
      | Exp '%' Exp                  { SInfix "%" $1 $3 }
      -- fixity 6
      | Exp '+' Exp                  { SInfix "+" $1 $3 }
      | Exp '-' Exp                  { SInfix "-" $1 $3 }
      | Exp ':+' Exp                 { SInfix ":+" $1 $3 }
      | Exp ':-' Exp                 { SInfix ":-" $1 $3 }
      -- fixity 5
      -- you know the drill by now
      | Exp '#' Exp                  { SInfix "#" $1 $3 }
      -- fixity 4
      | Exp '<$>' Exp                { SInfix "<$>" $1 $3 }
      | Exp '<*>' Exp                { SInfix "<*>" $1 $3 }
      | Exp '==' Exp                 { SInfix "==" $1 $3 }
      | Exp '!=' Exp                 { SInfix "!=" $1 $3 }
      | Exp '>' Exp                  { SInfix ">" $1 $3 }
      | Exp '<' Exp                  { SInfix "<" $1 $3 }
      | Exp '>=' Exp                 { SInfix ">=" $1 $3 }
      | Exp '<=' Exp                 { SInfix "<=" $1 $3 }
      -- fixity 3
      | Exp '&&' Exp                 { SInfix "&&" $1 $3 }
      -- fixity 2
      | Exp '||' Exp                 { SInfix "||" $1 $3 }
      -- fixity 1
      | Exp '>>=' Exp                { SInfix ">>=" $1 $3 }
      | Exp '|' Exp                  { SPipe $1 $3 }
      -- sections
      -- todo: implement `subtract` to allow `-` section
      | '(' BinOp Exp ')'            { SInfix $2 SHole $3 }
      | '(' Exp BinOp ')'            { SInfix $3 $2 SHole }
      | '(' BinOp ')'                { SInfix $2 SHole SHole }
      -- fixity 0
      | Exp '$' Exp                  { SApply $1 $3 }

App   : App Atom                     { SApply $1 $2 }
      | Atom                         { $1 }

Atom  : num                          { SNumber $1 }
      | bool                         { SBool $1 }
      | '(' ')'                      { SUnit }
      | ident                        { SIdentifier (S $1) }
      | builtin                      { SIdentifier (B $1) }
      | '_'                          { SHole }
      | '(' Exp ')'                  { SParens $2 }
      | '[' ListBody ']'             { SList $2 }

BinOp : '.' { "." }
      | '^' { "^" }
      | '*' { "*" }
      | '/' { "/" }
      | '%' { "%" }
      | '+' { "+" }
      | '-' { "-" }
      | ':+' { ":+" }
      | ':-' { ":-" }
      | '==' { "==" }
      | '/=' { "/=" }
      | '!=' { "!=" }
      | '>' { ">" }
      | '<' { "<" }
      | '>=' { ">=" }
      | '<=' { "<=" }
      | '<$>' { "<$>" }
      | '<*>' { "<*>" }
      | '&&' { "&&" }
      | '||' { "||" }
      | '>>=' { ">>=" }
      | '|' { "|" }
      | '$' { "$" }

{
parseError :: [Token] -> a
parseError = error "Parse error"
}
