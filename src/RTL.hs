{-# LANGUAGE DataKinds #-}
module RTL where
{-# LANGUAGE TypeOperators #-}

import Term
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import Prelude hiding (lookup)

data TRow = [Term] :- Term

instance Show TRow where
    show (ctx :- term) = show ctx ++ " :- " ++ show term

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

nodeGetTermT :: Node -> Term
nodeGetTermT node = let (_ :- t) = nodeGetTRow node in t

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

nodeGetTypo :: Node -> String
nodeGetTypo (InContext _) = "Ax"
nodeGetTypo (Eto _ _ _) = "E->"
nodeGetTypo (Ito _ _) = "I->"
nodeGetTypo (Iand _ _ _) = "I&"
nodeGetTypo (Eland _ _) = "El&"
nodeGetTypo (Erand _ _) = "Er&"
nodeGetTypo (Ilor _ _) = "Il|"
nodeGetTypo (Ilol _ _) = "Ir|"
nodeGetTypo (Eseq _ _ _ _) = "E|"
nodeGetTypo (Enotnot _ _) = "E!!"

getSonNodes :: Node -> [Node]
getSonNodes (InContext _) = []
getSonNodes (Eto _ a b) = [a, b]
getSonNodes (Ito _ a) = [a]
getSonNodes (Iand _ a b) = [a, b]
getSonNodes (Eland _ a) = [a]
getSonNodes (Erand _ a) = [a]
getSonNodes (Ilor _ a) = [a]
getSonNodes (Ilol _ a) = [a]
getSonNodes (Eseq _ a b c) = [a, b, c]
getSonNodes (Enotnot _ a) = [a]


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


insertInTerm :: Term -> Term -> Term -> Term
insertInTerm a b (V x)
  | x == "A" = a
  | x == "B" = b
  | otherwise = V x

insertInTerm a b (x :-> y) = insertInTerm a b x :-> insertInTerm a b y
insertInTerm a b (x `BAnd` y) = insertInTerm a b x `BAnd` insertInTerm a b y
insertInTerm a b (x `BOr` y) = insertInTerm a b x `BOr` insertInTerm a b y
insertInTerm _ _ BNOT = BNOT

insertInTRow :: Term -> Term -> TRow -> TRow
insertInTRow a b (g :- t) = map (insertInTerm a b) g :- insertInTerm a b t

insertInProof :: Term -> Term -> Node -> Node
insertInProof a b (InContext trow) = InContext (insertInTRow a b trow)
insertInProof a b (Eto trow a' b') = Eto (insertInTRow a b trow) (insertInProof a b a') (insertInProof a b b')
insertInProof a b (Ito trow a') = Ito (insertInTRow a b trow) (insertInProof a b a')
insertInProof a b (Iand trow a' b') = Iand (insertInTRow a b trow) (insertInProof a b a') (insertInProof a b b')
insertInProof a b (Eland trow a') = Eland (insertInTRow a b trow) (insertInProof a b a')
insertInProof a b (Erand trow a') = Erand (insertInTRow a b trow) (insertInProof a b a')
insertInProof a b (Ilor trow a') = Ilor (insertInTRow a b trow) (insertInProof a b a')
insertInProof a b (Ilol trow a') = Ilol (insertInTRow a b trow) (insertInProof a b a')
insertInProof a b (Eseq trow a' b' c') = Eseq (insertInTRow a b trow) (insertInProof a b a') (insertInProof a b b') (insertInProof a b c')
insertInProof a b (Enotnot trow a') = Enotnot (insertInTRow a b trow) (insertInProof a b a')



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