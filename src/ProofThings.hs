module ProofThings where

import Term
import RTL
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import Prelude hiding (lookup)
import EvalTerm
import Lemms
import Data.Set

getVarsUniq :: Term -> [String]
getVarsUniq = toList . getVars
    where
        getVars :: Term -> Set String
        getVars (V a) = singleton a
        getVars (a :-> b) = getVars a `union` getVars b
        getVars (a `BAnd` b) = getVars a `union` getVars b
        getVars (a `BOr` b) = getVars a `union` getVars b
        getVars BNOT = empty

getProof :: Map String Bool -> Term -> Node
getProof estimap t = proofHelper t
    where
        -- gi = (\x -> 
        --     case lookup x estimap 
        --         Just True -> V x 
        --         Just False -> tnot (V x)
        --         Nothing -> error "using getProof in wrong way") 
        -- <$> getVarsUniq t
        gi = (\x -> if lookup x estimap == Just True then V x else tnot (V x)) <$> getVarsUniq t

        proofHelper :: Term -> Node
        
        proofHelper (V a) = InContext (gi :- modulate estimap (V a))
        
        proofHelper BNOT = -- gi :- BNOT :-> BNOT
            Ito (gi :- (BNOT :-> BNOT)) $ 
            InContext ((BNOT : gi) :- BNOT)
         
        proofHelper alpha =
            let
                (a, b) = getAB alpha
                aProof = proofHelper a
                bProof = proofHelper b
                lol = getLemm estimap alpha -- одна из 14ти лемм
                lolWith = addToContext gi lol -- [=одна из 14ти лемм ] с добавлением gi в контекст по всему дереву
            in
            kwazar aProof bProof lolWith

getAB :: Term -> (Term, Term)
getAB (a :-> b) = (a, b)
getAB (a `BAnd` b) = (a, b)
getAB (a `BOr` b) = (a, b)
getAB _ = error "getAB: not a -> b"


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


