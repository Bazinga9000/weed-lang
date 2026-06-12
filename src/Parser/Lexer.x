{
module Parser.Lexer (Token(..), scanTokens) where
import AST (Builtin(..))
import Data.Char (isDigit)
import Text.Read (read)
import TowerNumber.Core (TowerNumber)
import TowerNumber.Parse (parseTN)

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
-- straight from the Haskell report, just in case we need wacky operators later
$symbol = [\! \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~ \: \#]

tokens :-
  $white+                       ; -- ignore whitespace
  "--".* ; -- ignore comments

  -- keywords
  "if"                          { \_ -> TokenIf }
  "then"                        { \_ -> TokenThen }
  "else"                        { \_ -> TokenElse }
  "let"                         { \_ -> TokenLet }
  "in"                          { \_ -> TokenIn }

  -- punctuation & reserved symbols
  "->"                          { \_ -> TokenArrow }
  "\"                           { \_ -> TokenLambda }
  "_"                           { \_ -> TokenHole }
  "("                           { \_ -> TokenLParen }
  ")"                           { \_ -> TokenRParen }
  "["                           { \_ -> TokenLBracket }
  "]"                           { \_ -> TokenRBracket }
  ","                           { \_ -> TokenComma }
  "_"                           { \_ -> TokenHole }
  ";"                           { \_ -> TokenSemi }

  -- hardcoded tokens
  "d%"                          { \_ -> TokenIdent "d%" }

  -- identifiers & literals
  True|False                    { \s -> TokenBool (read s) }
  $digit+ (\. $digit+)?         { \s -> TokenNum (parseTN s) }
  $alpha [$alpha $digit \_]* { \s -> TokenIdent s }

  -- operators
  $symbol+                      { \s -> TokenOp s }
{

data Token
  = TokenIf | TokenThen | TokenElse | TokenLet | TokenIn
  | TokenArrow | TokenLambda | TokenHole
  | TokenLParen | TokenRParen | TokenLBracket | TokenRBracket | TokenComma | TokenSemi
  | TokenBool Bool
  | TokenNum TowerNumber
  | TokenIdent String
  | TokenOp String
  | TokenBuiltin Builtin
  deriving (Eq, Show)

-- interceptor pass to make builtins into their proper tokens.
refineTokens :: [Token] -> [Token]
refineTokens [] = []
refineTokens (TokenIdent s : ts) = case s of
  -- builtin functions
  -- "keep"   -> TokenBuiltin Keep : refineTokens ts
  -- "drop"   -> TokenBuiltin Drop : refineTokens ts
  -- "sum"    -> TokenBuiltin Sum : refineTokens ts
  -- "highest" -> TokenBuiltin Highest : refineTokens ts

  -- primitive dice (standalone)
  "d"        -> TokenBuiltin DiceD : refineTokens ts
  "f"        -> TokenBuiltin DiceF : refineTokens ts
  "s"        -> TokenBuiltin DiceS : refineTokens ts
  "u"        -> TokenBuiltin DiceU : refineTokens ts
  "gauss"    -> TokenBuiltin DiceGauss : refineTokens ts
  "pareto"   -> TokenBuiltin DicePareto : refineTokens ts
  "binomial" -> TokenBuiltin DiceBinomial : refineTokens ts
  "coin"     -> TokenBuiltin DiceCoin : refineTokens ts
  "circle"   -> TokenBuiltin DiceCircle : refineTokens ts

  -- concatenated dice splitting (e.g. "d6", "d100", "d%")
  "d%" -> TokenBuiltin DiceD : TokenNum 100 : refineTokens ts
  "dF" -> TokenBuiltin DiceF : TokenNum 1 : refineTokens ts
  ('d':rest) | not (null rest) && all isDigit rest ->
      TokenBuiltin DiceD : TokenNum (parseTN rest) : refineTokens ts
  ('u':rest) | not (null rest) && all isDigit rest ->
      TokenBuiltin DiceU : TokenNum (parseTN rest) : refineTokens ts
  ('f':rest) | not (null rest) && all isDigit rest ->
      TokenBuiltin DiceF : TokenNum (parseTN rest) : refineTokens ts
  ('g':'a':'u':'s':'s':rest) | not (null rest) && all isDigit rest ->
      TokenBuiltin DiceGauss : TokenNum (parseTN rest) : refineTokens ts
  ('p':'a':'r':'e':'t':'o':rest) | not (null rest) && all isDigit rest ->
      TokenBuiltin DicePareto : TokenNum (parseTN rest) : refineTokens ts
  ('b':'i':'n':'o':'m':'i':'a':'l':rest) | not (null rest) && all isDigit rest ->
      TokenBuiltin DiceBinomial : TokenNum (parseTN rest) : refineTokens ts
  ('c':'i':'r':'c':'l':'e':rest) | not (null rest) && all isDigit rest ->
      TokenBuiltin DiceCircle : TokenNum (parseTN rest) : refineTokens ts

  -- just an identifier
  _        -> TokenIdent s : refineTokens ts

-- do nothing to everything else
refineTokens (t:ts) = t : refineTokens ts

scanTokens :: String -> [Token]
scanTokens str = refineTokens (alexScanTokens str)
}
