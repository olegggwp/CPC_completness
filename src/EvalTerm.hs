module EvalTerm where 
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
