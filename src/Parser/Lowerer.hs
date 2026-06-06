module Parser.Lowerer where

import AST
import Control.Applicative
import Control.Monad.RWS.CPS
import qualified Data.List as L
import Prelude hiding (Ap, Identity, Sum)

data LoweringError
  = TopLevelHole
  | BadUnaryOp String
  | BadBinaryOp String
  | InterpreterBug String
  deriving (Eq, Show)

type Lower = Either LoweringError

---
-- Primitive Pool desugaring
-- converts concatenated dice pools into Poolify infixes
-- 4d6 -> 4 # d6
---

isBuiltinDie :: IdentifierName -> Bool
isBuiltinDie (B DiceD) = True
isBuiltinDie (B DiceS) = True
isBuiltinDie (B DiceF) = True
isBuiltinDie (B DiceU) = True
isBuiltinDie (B DiceGauss) = True
isBuiltinDie (B DicePareto) = True
isBuiltinDie (B DiceBinomial) = True
isBuiltinDie (B DiceCoin) = True
isBuiltinDie (B DiceCircle) = True
isBuiltinDie _ = False

desugarPoolify :: SurfaceExpr -> SurfaceExpr
desugarPoolify e@(SApply (SApply (SNumber n) (SIdentifier i)) (SNumber p))
  | isBuiltinDie i = SInfix "#" (SNumber n) (SApply (SIdentifier i) (SNumber p))
  | otherwise = e -- errors in typechecker, but that's not the lowerer's problem
desugarPoolify (SApply (SNumber n) (SIdentifier (B DiceCoin))) = SInfix "#" (SNumber n) (SIdentifier (B DiceCoin)) -- special case for the only nullary primitive die DiceCoin
desugarPoolify (SNumber n) = SNumber n
desugarPoolify (SBool b) = SBool b
desugarPoolify SUnit = SUnit
desugarPoolify (SList es) = SList $ map desugarPoolify es
desugarPoolify (SIdentifier i) = SIdentifier i
desugarPoolify (SUnaryOp s e') = SUnaryOp s $ desugarPoolify e'
desugarPoolify (SInfix s e1 e2) = SInfix s (desugarPoolify e1) (desugarPoolify e2)
desugarPoolify (SParens e') = SParens $ desugarPoolify e'
desugarPoolify (SLambda ident body) = SLambda ident (desugarPoolify body)
desugarPoolify (SApply e1 e2) = SApply (desugarPoolify e1) (desugarPoolify e2)
desugarPoolify (SPipe e1 e2) = SPipe (desugarPoolify e1) (desugarPoolify e2)
desugarPoolify (SIf c t f) = SIf (desugarPoolify c) (desugarPoolify t) (desugarPoolify f)
desugarPoolify (SLet ident binding body) = SLet ident (desugarPoolify binding) (desugarPoolify body)
desugarPoolify SHole = SHole

---
-- Hole Lifting
-- converts holes into lambdas according to the spec's rules
-- no holes remain in the AST after this
---

-- type Lifter a = WriterT [Int] (StateT Int Lower) a
type Lifter a = RWST () [Int] Int Lower a

-- executes an action, captures any holes it emitted, and PREVENTS them
-- from bubbling up any further with listen/censor.
captureHoles :: Lifter a -> Lifter (a, [Int])
captureHoles action = censor (const []) (listen action)

liftHoles :: SurfaceExpr -> Lower SurfaceExpr
liftHoles expr = do
  (finalExpr, _, unresolvedHoles) <- runRWST (liftExpr expr) () 1

  case unresolvedHoles of
    [] -> Right finalExpr
    _ ->
      if hasInfixOrPipe expr
        then Right (wrapLambdas unresolvedHoles finalExpr)
        else Left TopLevelHole
  where
    liftExpr :: SurfaceExpr -> Lifter SurfaceExpr
    liftExpr SHole = do
      holeId <- get
      modify (+ 1)
      tell [holeId]
      return $ SIdentifier (U holeId)
    liftExpr (SNumber n) = return $ SNumber n
    liftExpr (SBool b) = return $ SBool b
    liftExpr SUnit = return SUnit
    liftExpr (SIdentifier name) = return $ SIdentifier name
    liftExpr (SList es) = do
      -- list literal is a hole capturing boundary
      (es', holes) <- captureHoles (mapM liftExpr es)
      return $ wrapLambdas holes (SList es')
    liftExpr (SParens e) = do
      -- parens are a hole capturing boundary if they enclose infix/pipe
      if hasInfixOrPipe e
        then do
          (e', holes) <- captureHoles (liftExpr e)
          return $ SParens (wrapLambdas holes e')
        else SParens <$> liftExpr e -- Let holes bubble naturally since this is just apps
    liftExpr (SPipe e1 e2) = do
      -- pipe limbs are hole boundaries
      (e1', h1) <- captureHoles (liftExpr e1)
      (e2', h2) <- captureHoles (liftExpr e2)
      return $ SPipe (wrapLambdas h1 e1') (wrapLambdas h2 e2')
    liftExpr (SLet ident binding body) = do
      -- let binding is a hole boundary
      (binding', h1) <- captureHoles (liftExpr binding)
      body' <- liftExpr body -- body holes bubble up naturally
      return $ SLet ident (wrapLambdas h1 binding') body'
    liftExpr (SLambda ident body) = do
      -- lambdas are hole boundaries
      (body', h) <- captureHoles (liftExpr body)
      return $ SLambda ident (wrapLambdas h body')
    liftExpr (SUnaryOp op e) = SUnaryOp op <$> liftExpr e
    liftExpr (SInfix op e1 e2) = liftA2 (SInfix op) (liftExpr e1) (liftExpr e2)
    liftExpr (SApply e1 e2) = liftA2 SApply (liftExpr e1) (liftExpr e2)
    liftExpr (SIf cond t branchF) =
      liftA3 SIf (liftExpr cond) (liftExpr t) (liftExpr branchF)

    wrapLambdas :: [Int] -> SurfaceExpr -> SurfaceExpr
    wrapLambdas holes e = foldr (SLambda . U) e holes

    -- intentionally stops at hole boundaries (parens, pipe, binop)
    hasInfixOrPipe :: SurfaceExpr -> Bool
    hasInfixOrPipe (SInfix {}) = True
    hasInfixOrPipe (SPipe _ _) = True
    hasInfixOrPipe (SUnaryOp _ e) = hasInfixOrPipe e
    hasInfixOrPipe (SApply e1 e2) = hasInfixOrPipe e1 || hasInfixOrPipe e2
    hasInfixOrPipe (SIf c t f) = hasInfixOrPipe c || hasInfixOrPipe t || hasInfixOrPipe f
    hasInfixOrPipe _ = False

---
-- Builtin Resolution
-- Converts identifiers to builtins (provided they are not shadowed)
---
builtinEnv :: [(String, Builtin)]
builtinEnv =
  [ ("negate", Negate),
    ("not", Not),
    ("add", Add),
    ("sub", Sub),
    ("mul", Mul),
    ("div", Div),
    ("mod", Mod),
    ("pow", Pow),
    ("floor", Floor),
    ("ceil", Ceil),
    ("and", And),
    ("or", Or),
    ("xor", Xor),
    ("id", Identity),
    ("map", Map),
    ("fmap", Map),
    ("ap", Ap),
    ("bind", Bind),
    -- dice are already builtins at this step
    ("constant", Constant),
    ("collapse", Collapse),
    ("source", Source),
    ("poolify", Poolify),
    ("sum", Sum)
  ]

resolveBuiltins :: SurfaceExpr -> Lower SurfaceExpr
resolveBuiltins expr = return $ runReader (resolveBuiltins' expr) []
  where
    resolveBuiltins' :: SurfaceExpr -> Reader [String] SurfaceExpr
    resolveBuiltins' (SNumber n) = return (SNumber n)
    resolveBuiltins' (SBool b) = return (SBool b)
    resolveBuiltins' SUnit = return SUnit
    resolveBuiltins' (SList es) = SList <$> mapM resolveBuiltins' es
    resolveBuiltins' (SIdentifier (B b)) = return (SIdentifier (B b))
    resolveBuiltins' (SIdentifier (U u)) = return (SIdentifier (U u))
    resolveBuiltins' (SIdentifier (S s)) = do
      ctx <- ask
      if s `elem` ctx
        then
          return (SIdentifier (S s))
        else case L.lookup s builtinEnv of
          Just b -> return (SIdentifier (B b))
          Nothing -> return (SIdentifier (S s))
    resolveBuiltins' (SUnaryOp s e) = SUnaryOp s <$> resolveBuiltins' e
    resolveBuiltins' (SInfix s e1 e2) = liftA2 (SInfix s) (resolveBuiltins' e1) (resolveBuiltins' e2)
    resolveBuiltins' (SParens e) = SParens <$> resolveBuiltins' e
    resolveBuiltins' (SLambda ident body) =
      let res = SLambda ident <$> resolveBuiltins' body
       in case ident of
            (B _) -> res
            (U _) -> res
            (S s) -> local (s :) res
    resolveBuiltins' (SApply e1 e2) = liftA2 SApply (resolveBuiltins' e1) (resolveBuiltins' e2)
    resolveBuiltins' (SPipe e1 e2) = liftA2 SPipe (resolveBuiltins' e1) (resolveBuiltins' e2)
    resolveBuiltins' (SIf cond t f) = liftA3 SIf (resolveBuiltins' cond) (resolveBuiltins' t) (resolveBuiltins' f)
    resolveBuiltins' (SLet ident binding body) =
      let res = liftA2 (SLet ident) (resolveBuiltins' binding) (resolveBuiltins' body)
       in case ident of
            (B _) -> res
            (U _) -> res
            (S s) -> do
              ctx <- ask
              if s `elem` ctx then res else local (s :) res
    resolveBuiltins' SHole = return SHole

---
-- operator dissolving
-- replace special surface nodes with their core equivalents
-- returns CoreUntypedExpr and thus is the final step
---
unaryOpToBuiltin :: String -> Lower Builtin
unaryOpToBuiltin "-" = Right Negate
unaryOpToBuiltin s = Left $ BadUnaryOp s

binaryOpToBuiltin :: String -> Lower Builtin
binaryOpToBuiltin "+" = Right Add
binaryOpToBuiltin "-" = Right Sub
binaryOpToBuiltin "*" = Right Mul
binaryOpToBuiltin "/" = Right Div
binaryOpToBuiltin "%" = Right Mod
binaryOpToBuiltin "^" = Right Pow
binaryOpToBuiltin "==" = Right Eq
binaryOpToBuiltin "/=" = Right Neq
binaryOpToBuiltin "!=" = Right Neq -- I know, I know.
binaryOpToBuiltin "<=" = Right Le
binaryOpToBuiltin "<" = Right Lt
binaryOpToBuiltin ">=" = Right Ge
binaryOpToBuiltin ">" = Right Gt
binaryOpToBuiltin "&&" = Right And
binaryOpToBuiltin "||" = Right Or
binaryOpToBuiltin "<$>" = Right Map
binaryOpToBuiltin "<*>" = Right Ap
binaryOpToBuiltin ">>=" = Right Bind
binaryOpToBuiltin "#" = Right Poolify
binaryOpToBuiltin s = Left $ BadBinaryOp s

-- dissolves special nodes no longer required after hole resolution
-- deletes: SUnaryOp, SInfix, SParens, SPipe, SIf
dissolveOps :: SurfaceExpr -> Lower CoreUntypedExpr
dissolveOps (SNumber n) = return $ CUNumber n
dissolveOps (SBool b) = return $ CUBool b
dissolveOps SUnit = return CUUnit
dissolveOps (SList xs) = CUList <$> mapM dissolveOps xs
dissolveOps (SIdentifier name) = return $ CUIdentifier name
dissolveOps (SUnaryOp op e) = do
  b <- unaryOpToBuiltin op
  e' <- dissolveOps e
  return $ CUApply (CUIdentifier (B b)) e'
dissolveOps (SInfix op e1 e2) = do
  b <- binaryOpToBuiltin op
  e1' <- dissolveOps e1
  e2' <- dissolveOps e2
  return $ CUApply (CUApply (CUIdentifier (B b)) e1') e2'
dissolveOps (SParens e) = dissolveOps e
dissolveOps (SLambda ident body) = CULambda ident <$> dissolveOps body
dissolveOps (SApply e1 e2) = liftA2 CUApply (dissolveOps e1) (dissolveOps e2)
dissolveOps (SPipe e2 e1) = dissolveOps (SApply e1 e2)
dissolveOps (SIf cond t f) = do
  cond' <- dissolveOps cond
  t' <- dissolveOps t
  f' <- dissolveOps f
  return $ CUApply (CUApply (CUApply (CUIdentifier (B If)) cond') t') f'
dissolveOps (SLet ident binding body) = liftA2 (CULet ident) (dissolveOps binding) (dissolveOps body)
dissolveOps SHole = Left $ InterpreterBug "Hole survived hole resolution"

lower :: SurfaceExpr -> Lower CoreUntypedExpr
lower e = liftHoles (desugarPoolify e) >>= resolveBuiltins >>= dissolveOps
