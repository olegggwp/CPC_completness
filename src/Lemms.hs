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
            "(A -> B) -> (!B -> (A -> B))"
            , "!B -> (A -> B)"
            , "!B -> (A -> !B)"
            , "(A -> B) -> ((A -> !B) -> !A)"
            , "((A -> B) -> ((A -> !B) -> !A)) -> (!B -> ((A -> B) -> ((A -> !B) -> !A)))"
            , "!B -> ((A -> B) -> ((A -> !B) -> !A))"
            , "(!B -> (A -> B)) -> ((!B -> ((A -> B) -> ((A -> !B) -> !A))) -> (!B -> ((A -> !B) -> !A)))"
            , "(!B -> ((A -> B) -> ((A -> !B) -> !A))) -> (!B -> ((A -> !B) -> !A))"
            , "!B -> ((A -> !B) -> !A)"
            , "(!B -> (A -> !B)) -> ((!B -> ((A -> !B) -> !A)) -> (!B -> !A))"
            , "(!B -> ((A -> !B) -> !A)) -> (!B -> !A)"
            , "!B -> !A",
            "!A->B",
            "(!A -> B) -> (!B -> (!A -> B))"
            , "!B -> (!A -> B)"
            , "!B -> (!A -> !B)"
            , "(!A -> B) -> ((!A -> !B) -> !!A)"
            , "((!A -> B) -> ((!A -> !B) -> !!A)) -> (!B -> ((!A -> B) -> ((!A -> !B) -> !!A)))"
            , "!B -> ((!A -> B) -> ((!A -> !B) -> !!A))"
            , "(!B -> (!A -> B)) -> ((!B -> ((!A -> B) -> ((!A -> !B) -> !!A))) -> (!B -> ((!A -> !B) -> !!A)))"
            , "(!B -> ((!A -> B) -> ((!A -> !B) -> !!A))) -> (!B -> ((!A -> !B) -> !!A))"
            , "!B -> ((!A -> !B) -> !!A)"
            , "(!B -> (!A -> !B)) -> ((!B -> ((!A -> !B) -> !!A)) -> (!B -> !!A))"
            , "(!B -> ((!A -> !B) -> !!A)) -> (!B -> !!A)"
            , "!B -> !!A",
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
    thowOnInvalidstr "sekLemm " $
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
            -- undefined -- a :- !!a
            else
                InContext $ [tnot a, tnot BNOT] :- (tnot a) -- !a :- !a
    else

        case (evalterm estimap a, evalterm estimap b) of
        (False, False) -> lemmTo00 a b

        (True, False)  -> lemmTo10 a b

        (False, True)  -> lemmTo01 a b

        (True, True)   -> lemmTo11 a b


getLemm estimap (a `BAnd` b) = thowOnInvalidstr ("BAND " ) $ case (evalterm estimap a, evalterm estimap b) of
    (False, False) -> thowOnInvalidstr ("BAND00 " ) $ lemmAnd00 a b
    (True, False)  -> thowOnInvalidstr ("BAND10 " ) $ lemmAnd10 a b
    (False, True)  -> thowOnInvalidstr ("BAND01 " ) $ lemmAnd01 a b
    (True, True)   -> thowOnInvalidstr ("BAND11 " ) $ lemmAnd11 a b

getLemm estimap (a `BOr` b) = case (evalterm estimap a, evalterm estimap b) of
    (False, False) -> lemmOr00 a b

    (True, False)  -> lemmOr10 a b

    (False, True)  -> lemmOr01 a b

    (True, True)   -> lemmOr11 a b


getLemm _  _= undefined

lemmOr11 :: Term -> Term -> Node
lemmOr11 a b = peremena a b $
    let xx = "A,B |- " in
          (xx ++) <$> [ "A", "A -> A | B", "A | B"]
        --   [ "A", "B",
        --     "A -> B -> A & B", 
        --     "B -> A & B", 
        --     "A & B"]

lemmOr01 :: Term -> Term -> Node
lemmOr01 a b = peremena a b $
    let xx = "!A,B |- " in
          (xx ++) <$> ["B", "B -> A | B", "A | B"]

lemmOr10 :: Term -> Term -> Node
lemmOr10 a b = peremena a b $
    let xx = "A,!B |- " in
          (xx ++) <$> [ "A", "A -> A | B", "A | B"]

lemmOr00 :: Term -> Term -> Node
lemmOr00 a b = peremena a b $
    [ "|- !A -> !B -> !(A|B)",
      "!A |- !B -> !(A|B)",
      "!A, !B |- !(A|B)"
    ]

lemmAnd11 :: Term -> Term -> Node
lemmAnd11 a b = peremena a b $
    let xx = "A,B |- " in
          (xx ++) <$>
          [ "A", "B",
            "A -> B -> A & B",
            "B -> A & B",
            "A & B"]

lemmAnd01 :: Term -> Term -> Node
lemmAnd01 a b = peremena a b $
    let xx = "!A,B |- " in
          (xx ++) <$>
    [ "!A", "!A -> A & B -> !A"
      , "A & B -> !A"
      , "A"
      , "A & B -> A"
      , "(A & B -> A) -> (A & B -> !A) -> !(A & B)"
      , "(A & B -> !A) -> !(A & B)"
      , "!(A & B)"
      ]

lemmAnd10 :: Term -> Term -> Node
lemmAnd10 a b = peremena a b $
    let xx = "A,!B |- " in
          (xx ++) <$>
    [ "!B","!B -> A & B -> !B"
      , "A & B -> !B"
      , "A & B -> B"
      , "(A & B -> B) -> (A & B -> !B) -> !(A & B)"
      , "(A & B -> !B) -> !(A & B)"
      , "!(A & B)"
      ]

lemmAnd00 :: Term -> Term -> Node
lemmAnd00 a b = peremena a b $
    let xx = "!A,!B |- " in
          (xx ++) <$>
    [ "!A", "!A -> A & B -> !A"
      , "A & B -> !A"
      , "A & B -> A"
      , "(A & B -> A) -> (A & B -> !A) -> !(A & B)"
      , "(A & B -> !A) -> !(A & B)"
      , "!(A & B)"
      ]

lemmTo01 :: Term -> Term -> Node
lemmTo01 a b =
    peremena a b $
    let xx = "!A,B |- " in
          (xx ++) <$>
          [
           "B"
          , "B -> A -> B"
          , "(A -> B)"
    ]

lemmTo11 :: Term -> Term -> Node
lemmTo11 a b =
    peremena a b $
    let xx = "A,B |- " in
          (xx ++) <$>
          [
           "B"
          , "B -> A -> B"
          , "(A -> B)"
    ]

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



lemmTo00 :: Term -> Term -> Node
lemmTo00 a b =
    peremena a b $
    let xx = "!A, !B, A |- " in
    [ xx ++ "A"
    , xx ++ "A -> !(A -> B) -> A"
    , xx ++ "!(A -> B) -> A"
    , xx ++ "!A"
    , xx ++ "!A -> !(A -> B) -> !A"
    , xx ++ "!(A -> B) -> !A"
    , xx ++ "(!(A -> B) -> A) -> (!(A -> B) -> !A) -> !!(A -> B)"
    , xx ++ "(!(A -> B) -> !A) -> !!(A -> B)"
    , xx ++ "!!(A -> B)"
    , xx ++ "!!(A -> B) -> (A -> B)"
    , xx ++ "A -> B"
    , xx ++ "B"
    , "!A, !B|- A -> B"
    ]


