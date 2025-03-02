module Main where
import Term
import Parser
import TrueChecker
import Data.Maybe (isNothing)
-- import OldX (main1)
import qualified LemmTests

main :: IO ()
-- main = OldX.main
main = LemmTests.test00
-- printSolution


-- main = do
--     input <- getLine
--     let parsed = parseFile input
--     case parsed of
--         Left err -> error $ show err
--         Right term -> do
--             putStrLn "Parsed term:"
--             let mbFalse = searchForFalse term
--             if isNothing mbFalse
--             then 
--                 -- putStrLn "Term is true"
--                 printSolution term
--             else do
--                 putStrLn $ "Formula is refutable " ++ show mbFalse
--             -- print term
--     putStrLn ""
