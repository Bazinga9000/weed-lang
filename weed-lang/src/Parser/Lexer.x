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
-- pool sugar: a number immediately followed by a concatenated die (e.g. 4 d6)
-- becomes the poolify infix 4 # d 6. This must fire before the TokenIdent
-- case below splits the die, so that 'keep f 4d6' parses as 'keep f (4#d6)'
-- rather than '(keep f 4) d 6'.
refineTokens (TokenNum n : TokenIdent s : ts)
  | Just (dieBlt, sides@(_:_)) <- parseConcatDie s =
      -- wrap in parens so the pool parses as an atom
      -- Only fires for dice with explicit sides (NdX) e.g 3coin is not a pool
      TokenLParen : TokenNum n : TokenOp "#" : TokenBuiltin dieBlt : TokenNum (parseTN sides) : TokenRParen : refineTokens ts
refineTokens (TokenIdent s : ts) = case parseConcatDie s of
  -- dice (bare "d", or concatenated "d6", "d100", "d%")
  Just (dieBlt, sides) ->
    -- wrap in parens similarly to the pool case for the same reason
    if null sides
      then TokenBuiltin dieBlt : refineTokens ts
      else TokenLParen : TokenBuiltin dieBlt : TokenNum (parseTN sides) : TokenRParen : refineTokens ts
  -- just an identifier
  Nothing -> TokenIdent s : refineTokens ts

-- do nothing to everything else
refineTokens (t:ts) = t : refineTokens ts

-- | Parse a die string into its builtin and sides. "d" -> (DiceD, "");
-- "d6" -> (DiceD, "6"); "d%" -> (DiceD, "100"); "coin" -> (DiceCoin, "").
-- Returns Nothing if it's not a die at all.
parseConcatDie :: String -> Maybe (Builtin, String)
parseConcatDie "d%" = Just (DiceD, "100")
parseConcatDie "dF" = Just (DiceF, "1")
parseConcatDie "d" = Just (DiceD, "")
parseConcatDie "f" = Just (DiceF, "")
parseConcatDie "s" = Just (DiceS, "")
parseConcatDie "u" = Just (DiceU, "")
parseConcatDie "coin" = Just (DiceCoin, "")
parseConcatDie ('d':rest) | all isDigit rest = Just (DiceD, rest)
parseConcatDie ('u':rest) | all isDigit rest = Just (DiceU, rest)
parseConcatDie ('f':rest) | all isDigit rest = Just (DiceF, rest)
parseConcatDie ('g':'a':'u':'s':'s':rest) | all isDigit rest = Just (DiceGauss, rest)
parseConcatDie ('p':'a':'r':'e':'t':'o':rest) | all isDigit rest = Just (DicePareto, rest)
parseConcatDie ('b':'i':'n':'o':'m':'i':'a':'l':rest) | all isDigit rest = Just (DiceBinomial, rest)
parseConcatDie ('c':'i':'r':'c':'l':'e':rest) | all isDigit rest = Just (DiceCircle, rest)
parseConcatDie _ = Nothing

scanTokens :: String -> [Token]
scanTokens str = refineTokens (alexScanTokens str)
}
