

module ProofThings where

import Term
import RTL
import Data.Map (Map)
import qualified Data.Map as Map
-- import Prelude hiding (lookup)
import EvalTerm
import Lemms
import Data.Set (Set)
import qualified Data.Set as Set
import PrintUtils (printTRow, printRef)
-- import Data.Set

getVarsUniq :: Term -> [String]
getVarsUniq = Set.toList . getVars
    where
        getVars :: Term -> Set String
        getVars (V a) = Set.singleton a
        getVars (a :-> b) = getVars a `Set.union` getVars b
        getVars (a `BAnd` b) = getVars a `Set.union` getVars b
        getVars (a `BOr` b) = getVars a `Set.union` getVars b
        getVars BNOT = Set.empty

getProof :: Map String Bool -> Term -> Node
getProof estimap t = proofHelper t
    where
        -- gi = (\x -> 
        --     case lookup x estimap 
        --         Just True -> V x 
        --         Just False -> tnot (V x)
        --         Nothing -> error "using getProof in wrong way") 
        -- <$> getVarsUniq t
        gi = (\x -> if Map.lookup x estimap == Just True then V x else tnot (V x)) <$> getVarsUniq t

        proofHelper :: Term -> Node

        proofHelper (V a) = thowOnInvalidstr "after VA" $ InContext (gi :- modulate estimap (V a))

        proofHelper BNOT = -- gi :- BNOT :-> BNOT
            thowOnInvalidstr "after BNOT" $
            Ito (gi :- (BNOT :-> BNOT)) $
            InContext ((BNOT : gi) :- BNOT)

        proofHelper alpha =
            let
                (a, b) = getAB alpha
                aProof = thowOnInvalidstr "after proofhelper a" $ proofHelper a
                bProof = thowOnInvalidstr "after proofhelper b" $ proofHelper b
                lol = thowOnInvalidstr "after getLemm" $ getLemm estimap alpha -- одна из 14ти лемм
            in
            thowOnInvalidstr ("after kwazar" ++ show alpha ++ " \nREF " ++ printRef estimap) $ kwazar aProof bProof lol







kwazar :: Node -> Node -> Node -> Node
kwazar na nb nlol =
    Eto (gi :- c) step1 nb
    where
        step1 = Eto (gi :- (b :-> c)) step2 na
        step2 = Ito (gi :- (a :-> b :-> c))
            -- $ thowOnInvalidstr ("kwazar bass" ++ 
            -- "\nNLOL: " ++ printTRow (nodeGetTRow nlol) ++ 
            -- "\nNA: " ++ printTRow (nodeGetTRow na) ++ 
            -- "\nNB: " ++ printTRow (nodeGetTRow nlol))
            $ Ito ((a : gi) :- (b :-> c))
            -- $ Ito (( b : a : gi) :- c) lolWith
            $ lolWith
        lolWith = addToContext gi nlol -- [=одна из 14ти лемм ] с добавлением gi в контекст по всему дереву
        (gi :- a) = nodeGetTRow na
        (_ :- b) = nodeGetTRow nb
        (_ :- c) = nodeGetTRow nlol


