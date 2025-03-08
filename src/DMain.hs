module Main where

import Term
import Parser
import TrueChecker
import Data.Maybe
-- import OldX (main1)
import qualified LemmTests
import PrintUtils (printNode, printRef)
import Reform (isToSc2, isToSc9, isToSc4, isToSc10, isToSc8, isToSc1, isToSc3, isToSc5, isToSc6, isToSc7)
import RTL (thowOnInvalidstr)

main :: IO ()
-- main = do
--     let xx = isToSc10 (V "A") []
--     printNode 0 $ xx
--     printNode 0 $ thowOnInvalidstr "haha" $ xx
-- main = LemmTests.testAll
-- printSolution (V "A", V "B") (V "A", V "B", V "C")


main = do
    input <- getLine
    let parsed = parseFile input
    case parsed of
        Left err -> error $ show err
        Right term -> do
            -- putStrLn "Parsed term:"
            let mbFalse = searchForFalse term
            case mbFalse of
                Nothing ->
                    printSolution term
                    -- putStrLn "Term is true"
                Just xx ->
                    putStrLn $ "Formula is refutable [" ++ printRef xx ++ "]"
            -- print term
    putStrLn ""
