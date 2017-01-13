module Main where
import Language.ToyML.Parser
import qualified Language.ToyML.Syntax.Parsed as P
import Language.ToyML.Syntax.Desugar(desugar)
import Language.ToyML.Eval
import Text.PrettyPrint.HughesPJClass
import Control.Monad
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        fs -> forM_ fs $ \f -> parseExprFile f >>= handle

handle :: Show e => Either e P.Exp -> IO ()
handle (Left err) = print err
handle (Right expr) = do
    putStrLn $ render (text "parsed:" <+> pPrint expr)
    let desugared = desugar expr
    putStrLn $ render (text "desugared:" <+> pPrint desugared)
    case evalTop desugared of
        Right v -> putStrLn $ "evaluated: " ++ show v
        Left err -> print err

repl :: IO ()
repl = forever $ getLine >>= handle . parseExpr
