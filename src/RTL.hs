{-# LANGUAGE DataKinds #-}
module RTL where

import Term
import Data.List (find)
import Control.Applicative ((<|>))
import OldX (arePermsEquivalent)

data TRow = [Term] :- Term
    -- deriving (Show)
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
  -- deriving (Show)

nodeGetTermT :: Node -> Term
nodeGetTermT node = let (_ :- t) = nodeGetTRow node in t

nodeGetCtx :: Node -> [Term]
nodeGetCtx node = let (ctx :- t) = nodeGetTRow node in ctx

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

getR :: Term -> Maybe Term
getR (a :-> b) = Just b
getR a = Nothing


assertNode :: Node -> Bool -> Maybe Node
assertNode node True = Nothing
assertNode node False = Just node

validateMe :: Node -> Maybe Node
validateMe x = Just x
-- validateMe (InContext (g :- term)) =
--   case find (== term) g of
--     Just _ -> Nothing
--     Nothing -> Just (InContext (g :- term))



-- validateMe me@(Eto (g :- psi) a b) = let
--     (g1 :- term1) = nodeGetTRow a
--     (g2 :- phi) = nodeGetTRow b
--     tr1 = if (phi :-> psi) == term1 
--       then Nothing 
--       else Just me
--     in validateMe a <|> validateMe b <|> tr1 <|> sameCtxWithSons me


-- validateMe me@(Ito (g :- tme) a) = let
--   (a1, b1) = getAB tme
--   (g1 :- psi) = nodeGetTRow a
--   tr0 = assertNode me $ isAtoB tme
--   tr1 = assertNode me $ b1 == psi && arePermsEquivalent g1 (a1 : g)
--   in validateMe a <|> tr0 <|> tr1

-- validateMe me@(Iand trow a b) = let
--   (_ :- met) = nodeGetTRow me
--   (g1 :- aa) = nodeGetTRow a
--   (g2 :- bb) = nodeGetTRow b
--   tr1 = if met == (aa `BAnd` bb) then Nothing else Just me
--   in validateMe a <|> validateMe b <|> sameCtxWithSons me <|> tr1

-- validateMe me@(Eland trow a) = validateMe a <|> sameCtxWithSons me
-- validateMe me@(Erand trow a) = validateMe a <|> sameCtxWithSons me
-- validateMe me@(Ilor trow a) = validateMe a <|> sameCtxWithSons me
-- validateMe me@(Ilol trow a) = validateMe a <|> sameCtxWithSons me
-- validateMe me@(Eseq trow a b c) = validateMe a <|> validateMe b <|> validateMe c
-- validateMe me@(Enotnot trow a) = validateMe a


isAtoB :: Term -> Bool
isAtoB (a :-> b) = True
isAtoB _ = False

getAandB :: Term -> (Term, Term)
getAandB (a `BAnd` b) = (a, b)
getAandB _ = error "not a and b"
-- arePermsEquivalent

sameCtxWithSons :: Node -> Maybe Node
sameCtxWithSons node = let 
  sonsG = nodeGetCtx <$> getSonNodes node
  myG = nodeGetCtx node
  in if all (arePermsEquivalent myG) sonsG then Nothing else Just node


-- thowOnInvalid :: Node -> Node
-- thowOnInvalid node = case validateMe node of
--     Just invalidNode -> error $ "lemm is invalid" ++ show (nodeGetTRow invalidNode)
--     Nothing -> node


thowOnInvalidstr :: String -> Node -> Node
thowOnInvalidstr str node = node
-- thowOnInvalidstr str node = case validateMe node of
--     Just invalidNode -> error $ str ++ " lemm is invalid :\n" ++ show (nodeGetTRow invalidNode) 
--       ++ " \nsons: " ++ show (nodeGetTRow <$> getSonNodes invalidNode)
--       ++ "\ntype: " ++ nodeGetTypo invalidNode
--     Nothing -> node