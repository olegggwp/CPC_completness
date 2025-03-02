module TrueChecker where
import RTL

import Data.Map (Map, lookup, fromList)
import Term
import Data.Set
import qualified Data.Map
import ProofThings
import EvalTerm
import PrintUtils (printNode)

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
    let estimaps = getEstimaps $ getVarsUniq term
    let proofs = (`getProof` term) <$> estimaps
    mapM_ (\x -> (x `printNode` 0) *> putStrLn "") proofs


    -- let trees = getTree term
    -- print term
    putStrLn "END"

