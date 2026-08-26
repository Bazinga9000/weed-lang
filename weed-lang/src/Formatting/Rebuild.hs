module Formatting.Rebuild (rebuild) where

import AST (Builtin (..))
import Data.List (maximum)
import Data.Text qualified as T
import Evaluator.Types (TraceEvent (..))
import Formatting.ANSI (ANSIForegroundColor (..), ANSIWeight (..), ansiFormatString, clearANSI)

-- the evaluator emits a flat sequence of events (this was rolled, this pool was
-- finalized, etc). This module converts this into a readable evaluation log by
-- parsing into a tree and then collapsing the tree one level at a time to get a
-- readable evaluation path

data LogExpr
  = ERoll Text -- a single die roll (a lone die, not part of a pool)
  | EPool [(Text, Bool)] (Maybe Text) -- a finalized pool: values (kept flags) and total
  | ELeaf Text -- a lifted scalar
  | EOp Builtin [LogExpr] Text -- an operator applied to its operands, and its result
  deriving (Show)

-- how many collapse steps before this node resolves to its result
collapseLevel :: LogExpr -> Int
collapseLevel (ERoll _) = 0
collapseLevel (ELeaf _) = 0
collapseLevel (EPool _ _) = 1
collapseLevel (EOp _ children _) = 1 + maximum (0 : map collapseLevel children)

opSymbol :: Builtin -> Text
opSymbol Add = "+"
opSymbol Sub = "-"
opSymbol Mul = "*"
opSymbol Div = "/"
opSymbol Mod = "%"
opSymbol Pow = "^"
opSymbol Negate = "-"
opSymbol _ = "?"

-- render a node at collapse level r. Nodes whose collapse level is <= r have
-- already resolved to their result value; the rest are shown structurally.
renderLogExpr :: Int -> LogExpr -> Text
renderLogExpr _ (ERoll v) = v
renderLogExpr _ (ELeaf v) = v
renderLogExpr r (EPool vals total) = if r == 0 then renderPool vals else maybe (renderPool vals) id total
renderLogExpr r e@(EOp b children result) =
  if r >= collapseLevel e then result else renderOp r b children

-- render a pool's contents.
renderPool :: [(Text, Bool)] -> Text
renderPool vals = "[" <> T.intercalate ", " (map renderItem vals) <> "]"
  where
    renderItem (v, True) = v
    renderItem (v, False) = ansiFormatString Gray Normal $ clearANSI v <> "×"

-- render a structurally-shown operator application, parenthesizing any child
-- that is itself an unresolved operator.
renderOp :: Int -> Builtin -> [LogExpr] -> Text
renderOp r b children = case children of
  [a] -> opSymbol b <> " " <> renderChild r a
  [a, c] -> renderChild r a <> " " <> opSymbol b <> " " <> renderChild r c
  _ -> T.intercalate ", " (map (renderChild r) children)

renderChild :: Int -> LogExpr -> Text
renderChild r child = case child of
  EOp _ _ _ | r < collapseLevel child -> "(" <> renderLogExpr r child <> ")"
  _ -> renderLogExpr r child

-- parse the flat event stream into a single LogExpression tree using a stack.
parseEvents :: [TraceEvent] -> [LogExpr]
parseEvents = go []
  where
    go stack [] = stack
    go stack (Rolled x : rest) = go (ERoll x : stack) rest
    -- a finalized pool: consume all preceding rolls and replace with the pool
    go stack (Pooled items total : rest) =
      let (_, more) = span isRoll stack
       in go (EPool items total : more) rest
    go stack (Lifted x : rest) = go (ELeaf x : stack) rest
    go stack (Applied b args result : rest) =
      let n = length args
          (operandsRev, more) = splitAt n stack
          operands = reverse operandsRev
       in go (EOp b operands result : more) rest

    isRoll (ERoll _) = True
    isRoll _ = False

-- rebuild the evaluation log from the trace, assuming it parses.
rebuild :: [TraceEvent] -> Maybe Text
rebuild evts = case parseEvents evts of
  [tree] ->
    let maxLvl = collapseLevel tree
        rendered = [renderLogExpr r tree | r <- [0 .. maxLvl]]
     in Just (T.unlines rendered)
  _ -> Nothing
