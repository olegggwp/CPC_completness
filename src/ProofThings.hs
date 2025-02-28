module ProofThings where

import Term
import RTL
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import Prelude hiding (lookup)


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
