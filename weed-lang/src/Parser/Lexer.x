{
module Parser.Lexer (Token(..), scanTokens) where
import AST (Builtin(..), MetaKind(..), AccessMode(..))
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

-- a keep/drop/explode suffix attached to a die expression.
data DieSuffix
  = KeepHighest Int
  | KeepLowest Int
  | DropLowest Int
  | DropHighest Int
  | ExplodeSuffix
  deriving (Eq, Show)

-- interceptor pass to desugar die expressions into their builtin forms.
refineTokens :: [Token] -> [Token]
refineTokens ts = case consumeDieExpr ts of
  Just (tokens, rest) -> tokens ++ refineTokens rest
  Nothing -> case ts of
    [] -> []
    (t : rest) -> t : refineTokens rest

-- try to consume a full sugared die expression at the head of the token
-- stream, returning the desugared tokens and the unconsumed remainder.
consumeDieExpr :: [Token] -> Maybe ([Token], [Token])
consumeDieExpr ts = case ts of
  -- pool
  (TokenNum n : TokenIdent s : rest)
    | Just (dieBlt, sides@(_ : _), suffixes) <- parseDieExpr s ->
        let base = TokenLParen : TokenNum n : TokenOp "#" : dieRawTokens dieBlt sides <> [TokenRParen]
         in Just (finishDie base suffixes rest)
  -- a single die
  (TokenIdent s : rest)
    | Just (dieBlt, sides, suffixes) <- parseDieExpr s ->
        let base = dieAtomTokens dieBlt sides
         in Just (finishDie base suffixes rest)
  _ -> Nothing

-- since ! splits the identifier, we need to check for ! followed by a suffix (kh2)
-- and make the full expr
finishDie :: [Token] -> [DieSuffix] -> [Token] -> ([Token], [Token])
finishDie base suffixes ts =
  let (explode, ts1) = case ts of
        (TokenOp "!" : r) -> (True, r)
        _ -> (False, ts)
      (kdSuffix, ts2) = case ts1 of
        (TokenIdent s : r) | Just suf <- parseStandaloneSuffix s -> ([suf], r)
        _ -> ([], ts1)
      allSuffixes = (if explode then [ExplodeSuffix] else []) <> suffixes <> kdSuffix
   in (buildSuffixes allSuffixes base, ts2)

-- construct the actual application by making a bunch of pipes
buildSuffixes :: [DieSuffix] -> [Token] -> [Token]
buildSuffixes [] base = base
buildSuffixes suffixes base =
  TokenLParen : foldl' addSuffix base suffixes <> [TokenRParen]
  where
    addSuffix acc s = acc <> [TokenOp "|"] <> suffixTokens s

-- convert a modifier into the tokens that apply it
suffixTokens :: DieSuffix -> [Token]
suffixTokens (KeepHighest n) = keepDropTokens Keep Highest n
suffixTokens (KeepLowest n) = keepDropTokens Keep Lowest n
suffixTokens (DropLowest n) = keepDropTokens Drop Lowest n
suffixTokens (DropHighest n) = keepDropTokens Drop Highest n
suffixTokens ExplodeSuffix =
  [ TokenBuiltin Explode,
    TokenLParen,
    TokenBuiltin (MetaAccess MKCrit AAny),
    TokenRParen
  ]

keepDropTokens :: Builtin -> Builtin -> Int -> [Token]
keepDropTokens keepDrop hiLo n =
  [ TokenBuiltin keepDrop,
    TokenLParen,
    TokenBuiltin hiLo,
    TokenNum (parseTN (show n)),
    TokenRParen
  ]

-- tokens for an atomic die (wrapped)
dieAtomTokens :: Builtin -> String -> [Token]
dieAtomTokens dieBlt sides
  | null sides = [TokenBuiltin dieBlt]
  | otherwise = TokenLParen : dieRawTokens dieBlt sides <> [TokenRParen]

-- tokens for an atomic die (unwrapped)
dieRawTokens :: Builtin -> String -> [Token]
dieRawTokens dieBlt sides = TokenBuiltin dieBlt : [TokenNum (parseTN sides) | not (null sides)]

-- parse an identifier to a triplet (primitive, sides, suffixes)
parseDieExpr :: String -> Maybe (Builtin, String, [DieSuffix])
parseDieExpr ('d' : 'F' : rest) = do
  suffixes <- parseSuffixes rest
  return (DiceF, "1", suffixes)
parseDieExpr s = do
  (dieBlt, rest) <- matchDieName s
  -- handle d% (d100) and dF (fudge d1) special sides
  let (sides, rest') = case rest of
        ('%' : r) -> ("100", r)
        _ -> span isDigit rest
  suffixes <- parseSuffixes rest'
  return (dieBlt, sides, suffixes)

-- match on a specific die primitive
matchDieName :: String -> Maybe (Builtin, String)
matchDieName ('d' : rest) = Just (DiceD, rest)
matchDieName ('f' : rest) = Just (DiceF, rest)
matchDieName ('s' : rest) = Just (DiceS, rest)
matchDieName ('u' : rest) = Just (DiceU, rest)
matchDieName ('c' : 'o' : 'i' : 'n' : rest) = Just (DiceCoin, rest)
matchDieName ('g' : 'a' : 'u' : 's' : 's' : rest) = Just (DiceGauss, rest)
matchDieName ('p' : 'a' : 'r' : 'e' : 't' : 'o' : rest) = Just (DicePareto, rest)
matchDieName ('b' : 'i' : 'n' : 'o' : 'm' : 'i' : 'a' : 'l' : rest) = Just (DiceBinomial, rest)
matchDieName ('c' : 'i' : 'r' : 'c' : 'l' : 'e' : rest) = Just (DiceCircle, rest)
matchDieName _ = Nothing

-- parse one or more suffixes
parseSuffixes :: String -> Maybe [DieSuffix]
parseSuffixes [] = Just []
parseSuffixes s = do
  (suf, rest) <- parseOneSuffix s
  (suf :) <$> parseSuffixes rest

-- parse one suffix (keep or drop)
parseOneSuffix :: String -> Maybe (DieSuffix, String)
parseOneSuffix ('k' : 'h' : rest) = readNumSuffix (KeepHighest) rest
parseOneSuffix ('k' : 'l' : rest) = readNumSuffix KeepLowest rest
parseOneSuffix ('d' : 'l' : rest) = readNumSuffix DropLowest rest
parseOneSuffix ('d' : 'h' : rest) = readNumSuffix DropHighest rest
parseOneSuffix ('k' : rest) = readNumSuffix KeepHighest rest
parseOneSuffix ('d' : rest) = readNumSuffix DropLowest rest
parseOneSuffix _ = Nothing

-- parse a standalone suffix (would appear after ! with no trailing content)
parseStandaloneSuffix :: String -> Maybe DieSuffix
parseStandaloneSuffix s = case parseOneSuffix s of
  Just (suf, "") -> Just suf
  _ -> Nothing

readNumSuffix :: (Int -> DieSuffix) -> String -> Maybe (DieSuffix, String)
readNumSuffix mk s = case span isDigit s of
  ("", _) -> Nothing
  (ds, rest) -> Just (mk (read ds), rest)

scanTokens :: String -> [Token]
scanTokens str = refineTokens (alexScanTokens str)
}
