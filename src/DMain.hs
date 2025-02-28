module Main where
import Term
import Parser
import TrueChecker
import Data.Maybe (isNothing)

main :: IO ()
main = do
    input <- getLine
    let parsed = parseFile input
    case parsed of
        Left err -> error $ show err
        Right term -> do
            putStrLn "Parsed term:"
            let mbFalse = searchForFalse term
            if isNothing mbFalse
            then putStrLn "Term is true"
            else do
                putStrLn $ "Formula is refutable " ++ show mbFalse
            print term

    putStrLn ""
