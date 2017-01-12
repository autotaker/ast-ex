module Main where
import Language.ToyML.Parser
import Text.PrettyPrint.HughesPJClass
import Control.Monad

main :: IO ()
main = forever $ do
    s <- getLine
    case parseExpr s of
        Left err -> print err
        Right expr -> putStrLn $ render (text ">" <+> pPrint expr)
