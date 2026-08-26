module Pipeline where

import Evaluator
import Evaluator.DropList
import Evaluator.WeedNumber (WeedNumber)
import Formatting.Pretty (prettyPrint)
import Formatting.Rebuild (rebuild)
import Parser.Lexer
import Parser.Lowerer
import Parser.SurfaceParser
import TypeChecker
import TypeChecker.Singletons (SWeedType (..))

-- the full pipeline for an expression.
-- returns (the pretty-printed result, its autosum, the rebuilt eval log).
interpret :: (ToString s) => s -> IO (Either Text (Text, Maybe Text, Maybe Text))
interpret input = do
  let toks = scanTokens $ toString input
  case parseSurface toks of
    Left e -> return $ Left $ "Parse error: " <> prettyPrint e
    Right surface -> case lower surface of
      Left e -> return $ Left $ "Lowering error: " <> prettyPrint e
      Right coreu -> case typeCheck coreu of
        Left e -> return $ Left $ "Type error: " <> prettyPrint e
        Right coret -> do
          ev <- evaluate coret
          case ev of
            Left err -> return $ Left $ "Evaluation error: " <> prettyPrint err
            Right (v, evts) ->
              let res = prettyPrint v
                  as = prettyPrint <$> autoSum v
                  log = rebuild evts
               in return $ Right (res, as, log)

-- collapses (nested) lists of numbers into their sum
autoSum :: SomeValue -> Maybe SomeValue
autoSum (SomeValue (STList _) (VList (DropList []))) = Nothing -- empty lists don't count
autoSum (SomeValue _ v) = SomeValue STNumber . VNumber <$> as' v
  where
    as' :: Value t -> Maybe WeedNumber
    as' (VNumber n) = Just n
    as' (VList l) = foldlM (\a b -> (+ a) <$> b) 0 $ map as' $ getKept l
    as' _ = Nothing
