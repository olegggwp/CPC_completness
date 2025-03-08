module Lemms where

import           Data.Either (lefts, rights)
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           EvalTerm
import           OldX
import           Reform      (reform, moveRight)
import           RTL
import           Term
import           Text.Parsec (parse)


sekLemmNode :: Node
sekLemmNode =
    let adder = "A->B, !A->B|-"
        p1 = (adder ++) <$>
            ["A->B",
            "(A -> B) -> (!B -> (A -> B))",
            "!B -> (A -> B)",
            "!B -> (A -> !B)",
            "(A -> B) -> ((A -> !B) -> !A)",
            "((A -> B) -> ((A -> !B) -> !A)) -> (!B -> ((A -> B) -> ((A -> !B) -> !A)))",
            "!B -> ((A -> B) -> ((A -> !B) -> !A))",
            "(!B -> (A -> B)) -> ((!B -> ((A -> B) -> ((A -> !B) -> !A))) -> (!B -> ((A -> !B) -> !A)))",
            "(!B -> ((A -> B) -> ((A -> !B) -> !A))) -> (!B -> ((A -> !B) -> !A))",
            "!B -> ((A -> !B) -> !A)",
            "(!B -> (A -> !B)) -> ((!B -> ((A -> !B) -> !A)) -> (!B -> !A))",
            "(!B -> ((A -> !B) -> !A)) -> (!B -> !A)",
            "!B -> !A",
            "!A->B",
            "(!A -> B) -> (!B -> (!A -> B))",
            "!B -> (!A -> B)",
            "!B -> (!A -> !B)",
            "(!A -> B) -> ((!A -> !B) -> !!A)",
            "((!A -> B) -> ((!A -> !B) -> !!A)) -> (!B -> ((!A -> B) -> ((!A -> !B) -> !!A)))",
            "!B -> ((!A -> B) -> ((!A -> !B) -> !!A))",
            "(!B -> (!A -> B)) -> ((!B -> ((!A -> B) -> ((!A -> !B) -> !!A))) -> (!B -> ((!A -> !B) -> !!A)))",
            "(!B -> ((!A -> B) -> ((!A -> !B) -> !!A))) -> (!B -> ((!A -> !B) -> !!A))",
            "!B -> ((!A -> !B) -> !!A)",
            "(!B -> (!A -> !B)) -> ((!B -> ((!A -> !B) -> !!A)) -> (!B -> !!A))",
            "(!B -> ((!A -> !B) -> !!A)) -> (!B -> !!A)",
            "!B -> !!A",
            "(!B -> !A) -> (!B -> !!A) -> !!B",
            "(!B -> !!A) -> !!B",
            "!!B",
            "!!B -> B",
            "B"]
        p2 = p1 ++ ["(A->B) |- (!A->B) -> B",
                    "|-(A->B) -> (!A->B) -> B"]
    in justPeremena p2

sekLemm :: Term -> Term -> Node
sekLemm a b =
    -- thowOnInvalidstr "sekLemm " $
    insertInProof a b $
    sekLemmNode




justPeremena :: [String] ->  Node
justPeremena ls =
    let
    parsed = map (parse ctxAndTermP "") ls
    solutions = firstSol $ rights parsed
    errors = lefts parsed
    twoTree = if null errors then reform $ accGetNode $ head solutions
    else
        error $ "First parse error: " ++ show (head errors)
    -- res = insertInProof a b 
    in twoTree


peremena :: Term -> Term -> [String] ->  Node
peremena a b ls =
    let
    parsed = map (parse ctxAndTermP "") ls
    solutions = firstSol $ rights parsed
    errors = lefts parsed
    twoTree = if null errors then reform $ accGetNode $ head solutions
    else
        error $ "First parse error: " ++ show (head errors)
    res = insertInProof a b twoTree
    in res


getLemm :: Map String Bool -> Term -> Node
getLemm estimap (a :-> b) =
    if b == BNOT then
        if evalterm estimap a then
            lemmTo10 a b
            else
                InContext $ [tnot a, tnot BNOT] :- tnot a -- !a :- !a
    else

        case (evalterm estimap a, evalterm estimap b) of
        (False, False) -> lemmTo00 a b
        (True, False)  -> lemmTo10 a b
        (False, True)  -> lemmTo01 a b
        (True, True)   -> lemmTo11 a b


getLemm estimap (a `BAnd` b) = case (evalterm estimap a, evalterm estimap b) of
    (False, False) -> insertInProof a b lemmAnd00Node
    (True, False)  -> insertInProof a b lemmAnd10Node
    (False, True)  -> insertInProof a b lemmAnd01Node
    (True, True)   -> insertInProof a b lemmAnd11Node

getLemm estimap (a `BOr` b) = case (evalterm estimap a, evalterm estimap b) of
    (False, False) -> insertInProof a b lemmOr00Node
    (True, False)  -> insertInProof a b lemmOr10Node
    (False, True)  -> insertInProof a b lemmOr01Node
    (True, True)   -> insertInProof a b lemmOr11Node


getLemm _  _= undefined


lemmTo01 :: Term -> Term -> Node
lemmTo01 a b = insertInProof a b lemmTo01Node

lemmTo11 :: Term -> Term -> Node
lemmTo11 a b = insertInProof a b lemmTo11Node

lemmTo00 :: Term -> Term -> Node
lemmTo00 a b = insertInProof a b lemmTo00Node

lemmOr11Node :: Node
lemmOr11Node = justPeremena $
    let xx = "A,B |- " in
          (xx ++) <$> [ "A", "A -> A | B", "A | B"]


lemmOr01Node :: Node
lemmOr01Node = justPeremena $
    let xx = "!A,B |- " in
          (xx ++) <$> ["B", "B -> A | B", "A | B"]

lemmOr10Node :: Node
lemmOr10Node = justPeremena $
    let xx = "A,!B |- " in
          (xx ++) <$> [ "A", "A -> A | B", "A | B"]

lemmOr00Node :: Node
lemmOr00Node = justPeremena $
    [ "|- !A -> !B -> !(A|B)",
      "!A |- !B -> !(A|B)",
      "!A, !B |- !(A|B)"
    ]

lemmAnd11Node :: Node
lemmAnd11Node = justPeremena $
    let xx = "A,B |- " in
          (xx ++) <$>
          [ "A", "B",
            "A -> B -> A & B",
            "B -> A & B",
            "A & B"]

lemmAnd01Node ::  Node
lemmAnd01Node = 
    Eto ([V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> BNOT)) (Eto ([V "A" :-> BNOT,V "B"] :- ((BAnd (V "A") (V "B") :-> (V "A" :-> BNOT)) :-> (BAnd (V "A") (V "B") :-> BNOT))) (Ito ([V "A" :-> BNOT,V "B"] :- ((BAnd (V "A") (V "B") :-> V "A") :-> ((BAnd (V "A") (V "B") :-> (V "A" :-> BNOT)) :-> (BAnd (V "A") (V "B") :-> BNOT)))) (Ito ([BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- ((BAnd (V "A") (V "B") :-> (V "A" :-> BNOT)) :-> (BAnd (V "A") (V "B") :-> BNOT))) (Ito ([BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> BNOT)) (Eto ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- BNOT) (Eto ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- (V "A" :-> BNOT)) (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> (V "A" :-> BNOT)))) (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- BAnd (V "A") (V "B")))) (Eto ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- V "A") (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> V "A"))) (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- BAnd (V "A") (V "B")))))))) (Ito ([V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> V "A")) (Eland ([BAnd (V "A") (V "B"),V "A" :-> BNOT,V "B"] :- V "A") (InContext ([BAnd (V "A") (V "B"),V "A" :-> BNOT,V "B"] :- BAnd (V "A") (V "B")))))) (Eto ([V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> (V "A" :-> BNOT))) (Ito ([V "A" :-> BNOT,V "B"] :- ((V "A" :-> BNOT) :-> (BAnd (V "A") (V "B") :-> (V "A" :-> BNOT)))) (Ito ([V "A" :-> BNOT,V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> (V "A" :-> BNOT))) (InContext ([V "A" :-> BNOT,BAnd (V "A") (V "B"),V "A" :-> BNOT,V "B"] :- (V "A" :-> BNOT))))) (InContext ([V "A" :-> BNOT,V "B"] :- (V "A" :-> BNOT))))
    -- justPeremena $
    -- let xx = "!A,B |- " in
    --       (xx ++) <$>
    -- [ "!A", "!A -> A & B -> !A",
    --   "A & B -> !A",
    --   "A",
    --   "A & B -> A",
    --   "(A & B -> A) -> (A & B -> !A) -> !(A & B)",
    --   "(A & B -> !A) -> !(A & B)",
    --   "!(A & B)"
    --   ]

lemmAnd10Node ::  Node
lemmAnd10Node = 
    Eto ([V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> BNOT)) (Eto ([V "A",V "B" :-> BNOT] :- ((BAnd (V "A") (V "B") :-> (V "B" :-> BNOT)) :-> (BAnd (V "A") (V "B") :-> BNOT))) (Ito ([V "A",V "B" :-> BNOT] :- ((BAnd (V "A") (V "B") :-> V "B") :-> ((BAnd (V "A") (V "B") :-> (V "B" :-> BNOT)) :-> (BAnd (V "A") (V "B") :-> BNOT)))) (Ito ([BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- ((BAnd (V "A") (V "B") :-> (V "B" :-> BNOT)) :-> (BAnd (V "A") (V "B") :-> BNOT))) (Ito ([BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> BNOT)) (Eto ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- BNOT) (Eto ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- (V "B" :-> BNOT)) (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> (V "B" :-> BNOT)))) (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- BAnd (V "A") (V "B")))) (Eto ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- V "B") (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> V "B"))) (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- BAnd (V "A") (V "B")))))))) (Ito ([V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> V "B")) (Erand ([BAnd (V "A") (V "B"),V "A",V "B" :-> BNOT] :- V "B") (InContext ([BAnd (V "A") (V "B"),V "A",V "B" :-> BNOT] :- BAnd (V "A") (V "B")))))) (Eto ([V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> (V "B" :-> BNOT))) (Ito ([V "A",V "B" :-> BNOT] :- ((V "B" :-> BNOT) :-> (BAnd (V "A") (V "B") :-> (V "B" :-> BNOT)))) (Ito ([V "B" :-> BNOT,V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> (V "B" :-> BNOT))) (InContext ([V "B" :-> BNOT,BAnd (V "A") (V "B"),V "A",V "B" :-> BNOT] :- (V "B" :-> BNOT))))) (InContext ([V "A",V "B" :-> BNOT] :- (V "B" :-> BNOT))))

    -- justPeremena $
    -- let xx = "A,!B |- " in
    --       (xx ++) <$>
    -- [ "!B","!B -> A & B -> !B",
    --   "A & B -> !B",
    --   "A & B -> B",
    --   "(A & B -> B) -> (A & B -> !B) -> !(A & B)",
    --   "(A & B -> !B) -> !(A & B)",
    --   "!(A & B)"
    --   ]

lemmAnd00Node :: Node
lemmAnd00Node = justPeremena $
    let xx = "!A,!B |- " in
          (xx ++) <$>
    [ "!A", "!A -> A & B -> !A",
      "A & B -> !A",
      "A & B -> A",
      "(A & B -> A) -> (A & B -> !A) -> !(A & B)",
      "(A & B -> !A) -> !(A & B)",
      "!(A & B)"
      ]

lemmTo01Node :: Node
lemmTo01Node  =
    justPeremena $
    let xx = "!A,B |- " in
          (xx ++) <$>
          [
           "B",
          "B -> A -> B",
          "(A -> B)"
    ]

lemmTo11Node :: Node
lemmTo11Node =
    justPeremena $
    let xx = "A,B |- " in
          (xx ++) <$>
          [
           "B",
           "B -> A -> B",
           "(A -> B)"
    ]

lemmTo00Node :: Node
lemmTo00Node =
    let
    xx = "!A, !B, A |- "
    res =   ((xx ++) <$> [ "A",
            "A -> !(A -> B) -> A",
            "!(A -> B) -> A",
            "!A",
            "!A -> !(A -> B) -> !A",
            "!(A -> B) -> !A",
            "(!(A -> B) -> A) -> (!(A -> B) -> !A) -> !!(A -> B)",
            "(!(A -> B) -> !A) -> !!(A -> B)",
            "!!(A -> B)",
            "!!(A -> B) -> (A -> B)",
            "A -> B",
            "B"])
                ++
             ["!A, !B|- A -> B"]

    in justPeremena res

lemmTo10 :: Term -> Term -> Node
lemmTo10 a b =
    let
    gs = [a:->b, a, b :-> BNOT]
    s1 = InContext (gs :- a)
    s2 = InContext (gs :- (a :-> b))
    s3 = InContext (gs :- (b :-> BNOT))
    s4 = Eto (gs :- b) s2 s1
    s5 = Eto (gs :- BNOT) s3 s4
    res = moveRight 1 s5
    in
    res
