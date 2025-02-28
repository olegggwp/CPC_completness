{-# LANGUAGE DataKinds #-}
module RTL where
{-# LANGUAGE TypeOperators #-}

import Term
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import Prelude hiding (lookup)



data TRow = [Term] :- Term


data Node =
    InContext TRow
  | Eto TRow Node Node
  | Ito TRow Node
  | Iand TRow Node Node
  | Eland TRow Node
  | Erand TRow Node
  | Ilor TRow Node
  | Ilol TRow Node
  | Eseq TRow Node Node Node
  | Enotnot TRow Node

nodeGetTRow :: Node -> TRow
nodeGetTRow (InContext trow) = trow
nodeGetTRow (Eto trow _ _) = trow
nodeGetTRow (Ito trow _) = trow
nodeGetTRow (Iand trow _ _) = trow
nodeGetTRow (Eland trow _) = trow
nodeGetTRow (Erand trow _) = trow
nodeGetTRow (Ilor trow _) = trow
nodeGetTRow (Ilol trow _) = trow
nodeGetTRow (Eseq trow _ _ _) = trow
nodeGetTRow (Enotnot trow _) = trow



evalterm :: Map String Bool -> Term -> Bool
evalterm estimap (V a) =
    case lookup a estimap of
        Just x -> x
        Nothing -> error "using evalterm in wrong way"

evalterm estimap (a :-> b) =
    case (evalterm estimap a, evalterm estimap b) of
        (True, False) -> False
        (_, _) -> True

evalterm estimap (a `BAnd` b) =
    case (evalterm estimap a, evalterm estimap b) of
        (True, True) -> True
        _ -> False

evalterm estimap (a `BOr` b) =
    case (evalterm estimap a, evalterm estimap b) of
        (True, _) -> True
        (_, True) -> True
        _ -> False

evalterm estimap BNOT = False


modulate :: Map String Bool -> Term -> Term
modulate estimap (V a) =
    case lookup a estimap of
        Just True -> V a
        Just False -> V a :-> BNOT
        Nothing -> error "using modulate in wrong way"

modulate estimap BNOT = BNOT :-> BNOT

modulate estimap term = if evalterm estimap term then term else term :-> BNOT


getProof :: Map String Bool -> Term -> Node
getProof estimap = proofHelper
    where
        gi = undefined
        proofHelper :: Term -> Node
        proofHelper (V a) = InContext (gi :- modulate estimap (V a))
        proofHelper BNOT = undefined
        proofHelper alpha@(a :-> b) =
            let
                aProof = proofHelper a
                bProof = proofHelper b
                lol = getLemm estimap alpha -- одна из 14ти лемм
            in
            kwazar aProof bProof lol

getLemm :: Map String Bool -> Term -> Node
getLemm estimap (a :-> b) = undefined
    where 
        a1 = modulate estimap a
        b1 = modulate estimap b
        c1 = modulate estimap (a :-> b)
        want = [a1,b1] :- c1
getLemm _  _= undefined

kwazar :: Node -> Node -> Node -> Node
kwazar na nb nlol =
    Eto (gi :- c) step1 nb
    where
        step1 = Eto (gi :- (b :-> c)) step2 na
        step2 = Ito (gi :- (a :-> b :-> c)) (Ito ((a : gi) :- (b :-> c)) (Ito (( b : a : gi) :- c) lolWith))
        lolWith = addToContext gi nlol -- [=одна из 14ти лемм ] с добавлением gi в контекст по всему дереву
        (gi :- a) = nodeGetTRow na
        (_ :- b) = nodeGetTRow nb
        (_ :- c) = nodeGetTRow nlol

addToContext :: [Term] -> Node -> Node
addToContext gi (InContext (g :- t)) = InContext ((g ++ gi) :- t)  
addToContext gi (Eto (g :- t) a b) = Eto ((g ++ gi) :- t) (addToContext gi a) (addToContext gi b)
addToContext gi (Ito (g :- t) a) = Ito ((g ++ gi) :- t) (addToContext gi a)
addToContext gi (Iand (g :- t) a b) = Iand ((g ++ gi) :- t) (addToContext gi a) (addToContext gi b)
addToContext gi (Eland (g :- t) a) = Eland ((g ++ gi) :- t) (addToContext gi a)
addToContext gi (Erand (g :- t) a) = Erand ((g ++ gi) :- t) (addToContext gi a)
addToContext gi (Ilor (g :- t) a) = Ilor ((g ++ gi) :- t) (addToContext gi a)
addToContext gi (Ilol (g :- t) a) = Ilol ((g ++ gi) :- t) (addToContext gi a)
addToContext gi (Eseq (g :- t) a b c) = Eseq ((g ++ gi) :- t) (addToContext gi a) (addToContext gi b) (addToContext gi c)
addToContext gi (Enotnot (g :- t) a) = Enotnot ((g ++ gi) :- t) (addToContext gi a)



-- getProof _ = undefined

-- data Node a where
--     InContext :: TRow -> Node x
--     Eto :: Node a -> Node b -> Node r
--     Ito :: Node a -> Node r
--     Iand :: Node a -> Node b -> Node r
--     Eland :: Node a -> Node r
--     Erand :: Node a -> Node r
--     Ilor :: Node a -> Node r
--     Ilol :: Node a -> Node r
--     Eseq :: Node a -> Node b -> Node c -> Node r
--     Enotnot :: Node a -> Node r


-- getRight :: Term -> Term
-- getRight (_ :-> b) = b
-- getRight _ = error "getRight: not a right term"

-- nodeGetTRow :: Node a -> TRow
-- nodeGetTRow (InContext trow) = trow
-- nodeGetTRow (Eto a _ ) = g :- getRight x
--     where
--         (g :- x) = nodeGetTRow a
-- nodeGetTRow (Ito a) = g :- getRight x
--     where
--         (g :- x) = nodeGetTRow a



-- getProof :: TRow -> Node
-- getProof t@(g :- (V x)) =
--     -- можно было бы сделать проверочку
--     InContext t
-- getProof t@(g :- BNOT) = 
--     InContext t

-- getProof (g :- (a :-> b)) = 
--     Eto (g :- (a :-> b)) xx (getProof (g :- b))
--     where
--         c = a :-> b
--         xx = Eto (g :- (b :-> c)) -- (getProof (g :- (b :-> c)))

-- getProof (g :- (a `BAnd` b)) = undefined

-- getProof (g :- (a `BOr` b)) = undefined