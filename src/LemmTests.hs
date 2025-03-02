module LemmTests where
import Term
import RTL
import Lemms
import qualified Data.Map as Map
import PrintUtils

test00 :: IO ()
test00 = do
    putStrLn "hello!"
    putStrLn $ prettyPrintNode $ getLemm (Map.fromList [("a", False), ("b", False)]) (V "a" :-> V "b")

test10 :: IO ()
test10 = do
    putStrLn "hello!"
    putStrLn $ prettyPrintNode $ getLemm (Map.fromList [("a", True), ("b", False)]) (V "a" :-> V "b")

test11 :: IO ()
test11 = do
    putStrLn "hello!"
    putStrLn $ prettyPrintNode $ getLemm (Map.fromList [("a", True), ("b", True)]) (V "a" :-> V "b")

test01 :: IO ()
test01 = do
    putStrLn "hello!"
    putStrLn $ prettyPrintNode $ getLemm (Map.fromList [("a", False), ("b", True)]) (V "a" :-> V "b")

testAnd11 :: IO ()
testAnd11 = do
    putStrLn "hello!"
    putStrLn $ prettyPrintNode $ getLemm (Map.fromList [("a", True), ("b", True)]) (V "a" `BAnd` V "b")


testAnd01 :: IO ()
testAnd01 = do
    putStrLn "hello!"
    putStrLn $ prettyPrintNode $ getLemm (Map.fromList [("a", False), ("b", True)]) (V "a" `BAnd` V "b")


testAnd10 :: IO ()
testAnd10 = do
    putStrLn "hello!"
    putStrLn $ prettyPrintNode $ getLemm (Map.fromList [("a", True), ("b", False)]) (V "a" `BAnd` V "b")

testAnd00 :: IO ()
testAnd00 = do
    putStrLn "hello!"
    putStrLn $ prettyPrintNode $ getLemm (Map.fromList [("a", False), ("b", False)]) (V "a" `BAnd` V "b")


testOr11 :: IO ()
testOr11 = do
    putStrLn "hello!"
    putStrLn $ prettyPrintNode $ getLemm (Map.fromList [("a", True), ("b", True)]) (V "a" `BOr` V "b")


testOr01 :: IO ()
testOr01 = do
    putStrLn "hello!"
    putStrLn $ prettyPrintNode $ getLemm (Map.fromList [("a", False), ("b", True)]) (V "a" `BOr` V "b")


testOr10 :: IO ()
testOr10 = do
    putStrLn "hello!"
    putStrLn $ prettyPrintNode $ getLemm (Map.fromList [("a", True), ("b", False)]) (V "a" `BOr` V "b")

testOr00 :: IO ()
testOr00 = do
    putStrLn "hello!"
    putStrLn $ prettyPrintNode $ getLemm (Map.fromList [("a", False), ("b", False)]) (V "a" `BOr` V "b")