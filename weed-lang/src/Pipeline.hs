module Pipeline where

import Evaluator
import Evaluator.Types
import Evaluator.DropList
import Formatting.Pretty (prettyPrint)
import Parser.Lexer
import Parser.Lowerer
import Parser.SurfaceParser
import TypeChecker

-- the full pipeline for an expression
interpret :: (ToString s) => s -> IO (Either Text (Text, Maybe Text))
interpret input = do
  let toks = scanTokens $ toString input
  case parseSurface toks of
    Left (ParseError ts) -> return $ Left $ "Parse error: " <> show ts
    Right surface -> case lower surface of
      Left e -> return $ Left $ "Lowering error: " <> show e
      Right coreu -> case typeCheck coreu of
        Left e -> return $ Left $ "Type error: " <> show e
        Right coret -> do
          ev <- evaluate coret
          case ev of
            Left err -> return $ Left $ "Evaluation error: " <> prettyPrint err
            Right v -> return $ Right (prettyPrint v, prettyPrint <$> autoSum v)

-- collapses (nested) lists of numbers into their sum
autoSum :: Value -> Maybe Value
autoSum (VList (DropList [])) = Nothing -- empty lists don't count
autoSum v = VNumber <$> as' v
  where
    as' (VNumber n) = Just n
    as' (VList l) = foldlM (\a b -> (+ a) <$> b) 0 $ map as' $ getKept l
    as' _ = Nothing
