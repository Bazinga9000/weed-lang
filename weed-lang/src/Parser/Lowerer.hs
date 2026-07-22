module Parser.Lowerer where

import AST
import Control.Monad.RWS.CPS
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (lookup)
import Data.Sequence qualified as S
import Prelude hiding (Ap, Identity, Sum)

data LoweringError
  = TopLevelHole
  | BadUnaryOp String
  | BadBinaryOp String
  | InterpreterBug String
  deriving (Eq, Show)

type Lower = Either LoweringError

---
-- Let binding toplogical sorting
-- the parser, dumb and lazy as it is, outputs all let bindings
-- as CULetRec. However, evaluating such bindings requires mFix
-- which will fall into an infinite loop if things need to be evaluated
-- strictly. To allow declarations like
--
-- let x = 1; y = x + y in y
--
-- which *should* evaluate just fine, we crack the big LetRec into nested Lets and LetRecs such that
-- only values which *actually* mutually depend on each other go into the same LetRec block
---

crackLets :: SurfaceExpr -> SurfaceExpr
crackLets (SNumber n) = SNumber n
crackLets (SBool b) = SBool b
crackLets SUnit = SUnit
crackLets (SList es) = SList $ crackLets <$> es
crackLets (SIdentifier ident) = SIdentifier ident
crackLets (SUnaryOp op e) = SUnaryOp op $ crackLets e
crackLets (SInfix op e1 e2) = SInfix op (crackLets e1) $ crackLets e2
crackLets (SParens e) = SParens $ crackLets e
crackLets (SLambda ident body) = SLambda ident $ crackLets body
crackLets (SApply e1 e2) = SApply (crackLets e1) (crackLets e2)
crackLets (SPipe e1 e2) = SPipe (crackLets e1) (crackLets e2)
crackLets (SIf c t f) = SIf (crackLets c) (crackLets t) (crackLets f)
crackLets SHole = SHole
crackLets (SLetRec decls finalBody) = foldr buildLet finalBody sccs
  where
    crackedDecls = map (\(Decl n e) -> Decl n $ crackLets e) decls
    graphNodes = [(decl, name, fv expr) | decl@(Decl name expr) <- crackedDecls]
    sccs = stronglyConnComp graphNodes

    buildLet :: SCC (Declaration SurfaceExpr) -> SurfaceExpr -> SurfaceExpr
    buildLet (AcyclicSCC decl) body = SLetRec [decl] body
    buildLet (CyclicSCC decls') body = SLetRec decls' body

    fv (SIdentifier ident) = [ident]
    fv (SList es) = es >>= fv
    fv (SUnaryOp _ e) = fv e
    fv (SInfix _ e1 e2) = fv e1 <> fv e2
    fv (SParens e) = fv e
    fv (SLambda ident body) = [x | x <- fv body, x /= ident]
    fv (SApply e1 e2) = fv e1 <> fv e2
    fv (SPipe e1 e2) = fv e1 <> fv e2
    fv (SIf c t f) = fv c <> fv t <> fv f
    fv (SLetRec decls' body) = [x | x <- allFvs, x `notElem` boundNames]
      where
        boundNames = [n | Decl n _ <- decls']
        allFvs = fv body <> concatMap (\(Decl _ e) -> fv e) decls'
    fv _ = []

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
desugarPoolify (SLetRec decls body) = SLetRec (map (fmap desugarPoolify) decls) (desugarPoolify body)
desugarPoolify SHole = SHole

---
-- Hole Lifting
-- converts holes into lambdas according to the spec's rules
-- no holes remain in the AST after this
---

-- type Lifter a = WriterT [Int] (StateT Int Lower) a
type Lifter a = RWST () (S.Seq Int) Int Lower a

-- executes an action, captures any holes it emitted, and PREVENTS them
-- from bubbling up any further with listen/censor.
captureHoles :: Lifter a -> Lifter (a, S.Seq Int)
captureHoles action = censor (const S.empty) (listen action)

liftHoles :: SurfaceExpr -> Lower SurfaceExpr
liftHoles expr = do
  (finalExpr, _, unresolvedHoles) <- runRWST (liftExpr expr) () 1

  case unresolvedHoles of
    S.Empty -> Right finalExpr
    _ ->
      if hasInfixOrPipe expr
        then Right (wrapLambdas unresolvedHoles finalExpr)
        else Left TopLevelHole
  where
    liftExpr :: SurfaceExpr -> Lifter SurfaceExpr
    liftExpr SHole = do
      holeId <- get
      modify (+ 1)
      tell $ one holeId
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
    liftExpr (SLetRec decls body) = do
      let liftDecl (Decl ident binding) =
            ( do
                -- let declarations are hole boundaries
                (binding', h1) <- captureHoles (liftExpr binding)
                return $ Decl ident (wrapLambdas h1 binding')
            )

      newDecls <- mapM liftDecl decls
      SLetRec newDecls <$> liftExpr body
    liftExpr (SLambda ident body) = do
      -- lambdas are hole boundaries
      (body', h) <- captureHoles (liftExpr body)
      return $ SLambda ident (wrapLambdas h body')
    liftExpr (SUnaryOp op e) = SUnaryOp op <$> liftExpr e
    liftExpr (SInfix op e1 e2) = liftA2 (SInfix op) (liftExpr e1) (liftExpr e2)
    liftExpr (SApply e1 e2) = liftA2 SApply (liftExpr e1) (liftExpr e2)
    liftExpr (SIf cond t branchF) =
      liftA3 SIf (liftExpr cond) (liftExpr t) (liftExpr branchF)

    wrapLambdas :: S.Seq Int -> SurfaceExpr -> SurfaceExpr
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
    ("mapP", MapP),
    ("ap", Ap),
    ("bind", Bind),
    ("liftMask", LiftMask),
    -- dice are already builtins at this step
    ("constant", Constant),
    ("collapse", Collapse),
    ("source", Source),
    ("poolify", Poolify),
    ("sum", Sum),
    ("keep", Keep),
    ("drop", Drop),
    ("explode", Explode),
    ("approximate", Approximate),
    ("highest", Highest),
    ("lowest", Lowest)
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
        else case lookup s builtinEnv of
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
    resolveBuiltins' (SLetRec decls body) = do
      newDecls <- mapM resolveOne decls
      newBody <- resolveBody body
      return $ SLetRec newDecls newBody
      where
        resolveOne (Decl ident binding) =
          let res = Decl ident <$> resolveBuiltins' binding
           in case ident of
                (B _) -> res
                (U _) -> res
                (S s) -> do
                  ctx <- ask
                  if s `elem` ctx then Decl ident <$> resolveBuiltins' binding else local (s :) res

        resolveBody b = do
          new <- fetchNewContext decls
          local (new <>) (resolveBuiltins' b)

        fetchNewContext [] = return []
        fetchNewContext ((Decl (B _) _) : rest) = fetchNewContext rest
        fetchNewContext ((Decl (U _) _) : rest) = fetchNewContext rest
        fetchNewContext ((Decl (S s) _) : rest) = (s :) <$> fetchNewContext rest

    -- resolveBuiltins' (SLetRec  ident binding body) =
    --   let res = liftA2 (SLetRec  ident) (resolveBuiltins' binding) (resolveBuiltins' body)
    --    in case ident of
    --         (B _) -> res
    --         (U _) -> res
    --         (S s) -> do
    --           ctx <- ask
    --           if s `elem` ctx then res else local (s :) res
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
binaryOpToBuiltin ":+" = Right ComplexAdd
binaryOpToBuiltin ":-" = Right ComplexSub
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
-- deletes: SUnaryOp, SInfix, SParens, SPipe
-- sugars single-declaration SLetRec into CULet
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
dissolveOps (SIf cond t f) = liftA3 CUIf (dissolveOps cond) (dissolveOps t) (dissolveOps f)
dissolveOps (SLetRec [Decl ident binding] body) = liftA2 CULet (Decl ident <$> dissolveOps binding) (dissolveOps body)
dissolveOps (SLetRec decls body) = do
  let dissolveDecl (Decl ident binding) = Decl ident <$> dissolveOps binding
  newDecls <- mapM dissolveDecl decls
  newBody <- dissolveOps body
  return $ CULetRec newDecls newBody
dissolveOps SHole = Left $ InterpreterBug "Hole survived hole resolution"

lower :: SurfaceExpr -> Lower CoreUntypedExpr
lower e = e & crackLets & desugarPoolify & liftHoles >>= resolveBuiltins >>= dissolveOps
