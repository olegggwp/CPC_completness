module TrueChecker where
import RTL

import Data.Map (Map, lookup, fromList)
import Term
import Data.Set
import qualified Data.Map


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


getVarsUniq :: Term -> [String]
getVarsUniq = toList . getVars
    where
        getVars :: Term -> Set String
        getVars (V a) = singleton a
        getVars (a :-> b) = getVars a `union` getVars b
        getVars (a `BAnd` b) = getVars a `union` getVars b
        getVars (a `BOr` b) = getVars a `union` getVars b
        getVars BNOT = empty

-- все возможные оценки переменных
getEstimaps :: [String] -> [Map String Bool]
getEstimaps names = Data.Map.fromList <$> getEstimaps' names
    where
        getEstimaps' :: [String] -> [[(String, Bool)]]
        getEstimaps' [] = [[]]
        getEstimaps' (v:vs) = [ (v, b) :  rest | b <- [True, False], rest <- getEstimaps' vs]

