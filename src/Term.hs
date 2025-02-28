-- {-# LANGUAGE DataKinds #-}
module Term  where

{-# LANGUAGE DeriveGeneric #-}



import           GHC.Generics        (Generic)

data Term =
      V String
    | Term :-> Term
    | BAnd Term Term
    | BOr Term Term
    | BNOT
    deriving (Eq, Generic)
infixr 2 :->
infixl 3 `BOr`
infixl 4 `BAnd`
-- Export the Term data type and its constructors
instance Show Term where
    show (V name)   = name
    show (a :-> b)  = "(" ++ show a ++ "->" ++ show b ++ ")"
    show (BAnd a b) = "(" ++ show a ++ "&" ++ show b ++ ")"
    show (BOr a b)  = "(" ++ show a ++ "|" ++ show b ++ ")"
    show BNOT   = "_|_"


type Row = ([Term], Term)


-- data Node x where
--     Eto :: Node (ctx, a :-> b) -> Node (ctx, a) -> Node (ctx, Term)

