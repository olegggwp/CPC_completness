-- -- {-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
-- -- {-# LANGUAGE TypeOperators #-}  -- Добавьте это расширение
-- -- {-# LANGUAGE DataKinds #-}  -- Добавьте это расширение

-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveGeneric #-}




-- module RoflsTree where
-- -- import Term

-- import           GHC.Generics        (Generic)


-- data TermType = TAtom | TImpl TermType TermType | TAnd TermType TermType | TOr TermType TermType | TNot TermType

-- -- Главный тип терма с параметром-видом
-- data Term (t :: TermType) where
--     V   :: String -> Term TAtom
--     (:->) :: Term a -> Term b -> Term (TImpl a b)
--     BAnd :: Term a -> Term b -> Term (TAnd a b)
--     BOr  :: Term a -> Term b -> Term (TOr a b)
--     BNot :: Term a -> Term (TNot a)

-- -- infixr 2 :->


-- infixr 2 :->
-- infixl 3 `BOr`
-- infixl 4 `BAnd`





-- data TRow = [Term] :- Term

-- data Node :: TRow -> * where
--     InContext :: TRow -> Node x
--     Eto :: Node (g :- (a :-> b)) -> Node (g :- a) -> Node (g :- b)
--     Ito :: Node (g :- b) -> Node (g1 :- (a :-> b))
--     Iand :: Node (g :- a) -> Node (g :- b) -> Node (g :- (a `BAnd` b))
--     Eland :: Node (g :- (a `BAnd` b)) -> Node (g :- a)
--     Erand :: Node (g :- (a `BAnd` b)) -> Node (g :- b)
--     Ilor :: Node (g :- a) -> Node (g :- (a `BOr` b))
--     Ilol :: Node (g :- b) -> Node (g :- (a `BOr` b))
--     Eseq :: Node (g1 :- ro) -> Node (g2 :- ro) -> Node (g :- (a `BOr` b)) -> Node (g :- ro)
--     Enotnot :: Node (g :- BNOT) -> Node (g1 :- a)



-- test1 :: Node a
-- test1 = InContext ([] :- (V "a" :-> V "b"))

-- test2 :: Node (g0 :- b0)
-- test2 = Eto (InContext ([] :- (V "a" :-> V "b"))) (InContext ([] :- V "a"))



-- termGetRight :: Term -> Term
-- termGetRight  = undefined
-- -- termGetRight (CId2 x) =  b

-- nodeGetTerm :: Node a -> a
-- nodeGetTerm (InContext (_ :- t)) = t
-- nodeGetTerm (Eto f s) = x
--     where
--         x = nodeGetTerm f