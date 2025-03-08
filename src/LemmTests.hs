module LemmTests where
import Term
import RTL
import Lemms
import qualified Data.Map as Map
import PrintUtils


testTemplate :: Map.Map String Bool -> Term -> IO ()
testTemplate env term = do
    -- putStrLn "hello!"
    let name = show term ++ " " ++ show env
    let noda1 = getLemm env term
    putStrLn $ "Testing:\t\t" ++ printTRow (nodeGetTRow noda1)
    putStrLn $ prettyPrintNode $ noda1
    let noda = thowOnInvalidstr ("TEST FACILITY NO: " ++ name) noda1
    -- putStrLn $ prettyPrintNode $ noda
    putStrLn $ printTRow (nodeGetTRow noda) ++ "\t[OK]"

test00, test10, test11, test01, testAnd11, testAnd01, testAnd10, testAnd00, testOr11, testOr01, testOr10, testOr00 :: IO ()
test00 = testTemplate (Map.fromList [("a", False), ("b", False)]) (V "a" :-> V "b")
test10 = testTemplate (Map.fromList [("a", True), ("b", False)]) (V "a" :-> V "b")
test11 = testTemplate (Map.fromList [("a", True), ("b", True)]) (V "a" :-> V "b")
test01 = testTemplate (Map.fromList [("a", False), ("b", True)]) (V "a" :-> V "b")
testAnd11 = testTemplate (Map.fromList [("a", True), ("b", True)]) (V "a" `BAnd` V "b")
testAnd01 = testTemplate (Map.fromList [("a", False), ("b", True)]) (V "a" `BAnd` V "b")
testAnd10 = testTemplate (Map.fromList [("a", True), ("b", False)]) (V "a" `BAnd` V "b")
testAnd00 = testTemplate (Map.fromList [("a", False), ("b", False)]) (V "a" `BAnd` V "b")
testOr11 = testTemplate (Map.fromList [("a", True), ("b", True)]) (V "a" `BOr` V "b")
testOr01 = testTemplate (Map.fromList [("a", False), ("b", True)]) (V "a" `BOr` V "b")
testOr10 = testTemplate (Map.fromList [("a", True), ("b", False)]) (V "a" `BOr` V "b")
testOr00 = testTemplate (Map.fromList [("a", False), ("b", False)]) (V "a" `BOr` V "b")

testAll :: IO ()
testAll = do
    test00
    test10
    test11
    test01
    testAnd11
    testAnd01
    testAnd10
    testAnd00
    testOr11
    testOr01
    testOr10
    testOr00
    putStrLn "All tests passed!"