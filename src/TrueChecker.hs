module TrueChecker where
import RTL

import Data.Map (Map, fromList)
import Term
-- import Data.Set
import qualified Data.Map
import ProofThings
import EvalTerm
import PrintUtils (printNode)
import Lemms (sekLemm)
import Data.List (find)

searchForFalse :: Term -> Maybe (Map String Bool)
searchForFalse term = let
    estimaps = getEstimaps $ getVarsUniq term
    in searchForFalse' estimaps term
    where
        searchForFalse' :: [Map String Bool] -> Term -> Maybe (Map String Bool)
        searchForFalse' [] _ = Nothing
        searchForFalse' (estimap:estimaps) term =
            if not $ evalterm estimap term
            then Just estimap
            else searchForFalse' estimaps term




-- все возможные оценки переменных
getEstimaps :: [String] -> [Map String Bool]
getEstimaps names = Data.Map.fromList <$> getEstimaps' names
    where
        getEstimaps' :: [String] -> [[(String, Bool)]]
        getEstimaps' [] = [[]]
        getEstimaps' (v:vs) = [ (v, b) :  rest | b <- [True, False], rest <- getEstimaps' vs]

printSolution :: Term -> IO ()
printSolution term = do
    -- let noda = optimizeMe $ mm (getVarsUniq term) []
    let noda = mm (getVarsUniq term) []
    printNode 0 noda


    where
        mm :: [String] -> [(String, Bool)] -> Node
        mm [] estimap = getProof (Data.Map.fromList estimap) term
        mm (a : xs) estimap = let
            va = V a
            n1 = mm xs $ (a, True) : estimap
            n2 = mm xs $ (a, False) : estimap
            gi = gadded <$> estimap
            lm = addToContext gi $ sekLemm va term
            mp1 =  Eto (gi :- (((tnot va) :-> term) :-> term)) lm
                $ Ito (gi :- (va :-> term))
                $ n1
            mp2 =  Eto (gi :- term) mp1
                $ Ito (gi :- ((tnot va) :-> term))
                $ n2
            in mp2

        gadded :: (String, Bool) -> Term
        gadded (x, True) = V x
        gadded (x, False) = tnot $ V x

    -- putStrLn "END"

    -- let trees = getTree term
    -- print term


optimizeMe :: Node -> Node
optimizeMe noda = let
    (g :- term) = nodeGetTRow noda
    optimizedG = find (== term) g
    opt = case optimizedG of
        Just _ -> InContext (g :- term)
        Nothing -> godeep noda
    in opt
    where
        godeep :: Node -> Node
        godeep (InContext trow) = InContext trow
        godeep (Eto trow a b) = Eto trow (optimizeMe a) (optimizeMe b)
        godeep (Ito trow a) = Ito trow (optimizeMe a)
        godeep (Iand trow a b) = Iand trow (optimizeMe a) (optimizeMe b)
        godeep (Eland trow a) = Eland trow (optimizeMe a)
        godeep (Erand trow a) = Erand trow (optimizeMe a)
        godeep (Ilor trow a) = Ilor trow (optimizeMe a)
        godeep (Ilol trow a) = Ilol trow (optimizeMe a)
        godeep (Eseq trow a b c) = Eseq trow (optimizeMe a) (optimizeMe b) (optimizeMe c)
        godeep (Enotnot trow a) = Enotnot trow (optimizeMe a)




