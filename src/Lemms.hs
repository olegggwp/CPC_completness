module Lemms where

import           Data.Either (lefts, rights)
import           Data.Map    (Map, lookup)
import qualified Data.Map    as Map
import           EvalTerm
import           OldX
import           Prelude     hiding (lookup)
import           Reform      (reform, moveRight)
import           RTL
import           Term
import           Text.Parsec (parse)



peremena :: Term -> Term -> [String] ->  Node
peremena a b ls =
    let
    parsed = map (parse contextAndTermP "") ls
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
                InContext $ [tnot a] :- (tnot a) -- !a :- !a
    else

        case (evalterm estimap a, evalterm estimap b) of
        (False, False) -> lemmTo00 a b

        (True, False)  -> lemmTo10 a b

        (False, True)  -> lemmTo01 a b

        (True, True)   -> lemmTo11 a b


getLemm estimap (a `BAnd` b) = case (evalterm estimap a, evalterm estimap b) of
    (False, False) -> lemmAnd00 a b

    (True, False)  -> lemmAnd10 a b

    (False, True)  -> lemmAnd01 a b

    (True, True)   -> lemmAnd11 a b

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
lemmTo10 a b = let
    gs = [a:->b, a, b :-> BNOT]
    s1 = InContext (gs :- a)
    s2 = InContext (gs :- (a :-> b))
    s3 = InContext (gs :- (b :-> BNOT))
    s4 = Eto (gs :- b) s2 s1
    s5 = Eto (gs :- BNOT) s3 s4
    res = moveRight 1 s5 
    in 
    res
    -- peremena a b $
    -- let xx = "A,!B |- " in
    --       ["A,!B |- "]


    -- peremena a b $
    -- let xx = "A,!B |- " in
    --       ((xx ++) <$>
    --       [
    --         "A"
    --       , "!B"
    --       , "!B -> (A -> B) -> !B"
    --       , "(A -> B) -> !B"
    --       , "((A -> B) -> A) -> ((A -> B) -> (A -> B)) -> ((A -> B) -> B)"
    --       , "A -> (A -> B) -> A"
    --       , "(A -> B) -> A"
    --       , "((A -> B) -> (A -> B)) -> ((A -> B) -> B)"
    --       ])
    --       ++
    --       [ "A,!B, (A -> B) |-  (A -> B) "
    --       , "A,!B |- ((A -> B) -> (A -> B)) "]
    --         ++
    --     ((xx ++) <$>
    --     [
    --        "(A -> B) -> B"
    --       , "((A -> B) -> B) -> ((A -> B) -> !B) -> !(A -> B)"
    --       , "((A -> B) -> !B) -> !(A -> B)"
    --       , "!(A -> B)"
    --     ])



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



-- hardLemm1 :: Term -> Term -> Node
-- hardLemm1 a b =
--     let
--     g1 = [a:->BNOT, b :-> BNOT, a]
--     s1 = InContext $ g1 :- a
-- --   [ "A"
--     s2 = sc1 g1 a (tnot (a :-> b))
-- --   , "A -> !(A -> B) -> A"
--     s3 = Eto (g1 :- ((tnot (a:->b)) :-> a)) s2 s1
-- --   , "!(A -> B) -> A"
--     s4 = InContext $ g1 :- tnot a
-- --   , "!A"
--     s5 = sc1 g1 (tnot a) (tnot (a :-> b))
-- --   , "!A -> !(A -> B) -> !A"
--     s6 = Eto (g1 :- (tnot (a:->b) :-> tnot a)) s5 s4
-- --   , "!(A -> B) -> !A"
--     s7 = sc9 g1 (tnot (a:->b)) a
-- --   , "(!(A -> B) -> A) -> (!(A -> B) -> !A) -> !!(A -> B)"
--     s8 = Eto (g1 :- (((tnot (a :-> b)) :-> tnot a) :-> (tnot (tnot (a :-> b))))) s7 s3
-- --   , "(!(A -> B) -> !A) -> !!(A -> B)"
--     s9 = Eto (g1 :- (tnot (tnot (a :-> b)))) s8 s6
-- --   , "!!(A -> B)"
--     s10 = sc10 g1 (a :-> b)
-- --   , "!!(A -> B) -> (A -> B)"
--     s11 = Eto (g1 :- (a :-> b)) s10 s9
-- --   , "A -> B"
--     s12 = Eto (g1 :- b) s11 s1
-- --   , "B"
--     in
--     s12



