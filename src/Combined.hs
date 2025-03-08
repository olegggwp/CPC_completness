{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main where


import           Control.Applicative ((<|>))
import           Control.Monad       (void)
import Data.Either ( lefts, rights )
import           Data.List

import Data.Map (Map, fromList)
import Data.Maybe
import Data.Maybe (fromMaybe)
import           Data.Maybe          (isJust)
import Data.Set (Set)
import Debug.Trace
import           GHC.Generics        (Generic)
import qualified Data.Map
import qualified Data.Map    as Map
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Text.Parsec.Expr
import           Text.Parsec         hiding ((<|>))
import           Text.Parsec (parse)
import           Text.Parsec.String  (Parser)
{-# LANGUAGE OverloadedStrings #-}


evalterm :: Map String Bool -> Term -> Bool
evalterm estimap (V a) =
    case Map.lookup a estimap of
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
    case Map.lookup a estimap of
        Just True -> V a
        Just False -> V a :-> BNOT
        Nothing -> error "using modulate in wrong way"

modulate estimap BNOT = BNOT :-> BNOT

modulate estimap term = if evalterm estimap term then term else term :-> BNOT


-- === СОДЕРЖИМОЕ ФАЙЛА EvalTerm.hs ===



testTemplate :: Map.Map String Bool -> Term -> IO ()
testTemplate env term = do
    -- putStrLn "hello!"
    let name = show term ++ " " ++ show env
    let noda1 = getLemm env term
    putStrLn $ "Testing:\t\t" ++ printTRow (nodeGetTRow noda1)
    putStrLn $ prettyPrintNode $ noda1
    let noda = thowOnInvalidstr ("TEST FACILITY NO: " ++ name) noda1
    -- putStrLn $ prettyPrintNode $ noda
    putStrLn $ printTRow (nodeGetTRow noda) ++ "\t[OK]"

test00, test10, test11, test01, testAnd11, testAnd01, testAnd10, testAnd00, testOr11, testOr01, testOr10, testOr00 :: IO ()
test00 = testTemplate (Map.fromList [("a", False), ("b", False)]) (V "a" :-> V "b")
test10 = testTemplate (Map.fromList [("a", True), ("b", False)]) (V "a" :-> V "b")
test11 = testTemplate (Map.fromList [("a", True), ("b", True)]) (V "a" :-> V "b")
test01 = testTemplate (Map.fromList [("a", False), ("b", True)]) (V "a" :-> V "b")
testAnd11 = testTemplate (Map.fromList [("a", True), ("b", True)]) (V "a" `BAnd` V "b")
testAnd01 = testTemplate (Map.fromList [("a", False), ("b", True)]) (V "a" `BAnd` V "b")
testAnd10 = testTemplate (Map.fromList [("a", True), ("b", False)]) (V "a" `BAnd` V "b")
testAnd00 = testTemplate (Map.fromList [("a", False), ("b", False)]) (V "a" `BAnd` V "b")
testOr11 = testTemplate (Map.fromList [("a", True), ("b", True)]) (V "a" `BOr` V "b")
testOr01 = testTemplate (Map.fromList [("a", False), ("b", True)]) (V "a" `BOr` V "b")
testOr10 = testTemplate (Map.fromList [("a", True), ("b", False)]) (V "a" `BOr` V "b")
testOr00 = testTemplate (Map.fromList [("a", False), ("b", False)]) (V "a" `BOr` V "b")

testAll :: IO ()
testAll = do
    test00
    test10
    test11
    test01
    testAnd11
    testAnd01
    testAnd10
    testAnd00
    testOr11
    testOr01
    testOr10
    testOr00
    putStrLn "All tests passed!"


-- === СОДЕРЖИМОЕ ФАЙЛА LemmTests.hs ===

{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE Strict #-}

-- import           Data.List
-- import           Data.List (foldr)


searchForTerm :: Term -> [Term] -> Bool
searchForTerm = elem



isFromOrig :: NodeX Bool -> [Term] -> Bool
isFromOrig node was =
    searchForTerm (nodeGetTerm node) was


printAboutOrig :: NodeX Bool -> [Term] -> String
printAboutOrig node was =
    if isFromOrig node was then " [from Original proof]\n" else "\n"


-- whitespace :: Parser ()
-- whitespace = skipMany (oneOf " \t\r")

-- lexeme :: Parser a -> Parser a
-- lexeme p = p <* whitespace

-- symbol :: String -> Parser String
-- symbol = lexeme . string

variableX :: Parser Term
variableX = lexeme $ do
    first <- letter
    rest <- many (letter <|> digit <|> char '\'')
    return $ V (first:rest)

termXP :: Parser Term
termXP = buildExpressionParser table term
  where
    table = [ [Prefix (tnot <$ symbol "!")]
            , [Infix  (BAnd <$ symbol "&") AssocLeft]
            , [Infix  (BOr <$ symbol "|") AssocLeft]
            , [Infix  ((:->) <$ symbol "->") AssocRight]
            ]

    term = prefixNot <|> parens termXP <|> variableX 
    parens = between (symbol "(") (symbol ")")
    prefixNot = do
        nots <- many1 (symbol "!")
        t <- term
        return $ foldr (const tnot) t nots

parseTermXX :: String -> Either ParseError Term
parseTermXX = parse (whitespace *> termXP <* eof) "Expression"

contextXP :: Parser [Term]
contextXP = sepBy termXP (symbol ",")

contextStrXP :: Parser String
contextStrXP = manyTill anyChar (try (string "|-"))

ctxAndTermP :: Parser Row
ctxAndTermP = do
    _ <- many space
    ctxStr <- contextStrXP
    _ <- many space
    expr <- termXP
    let ctx = case parse contextXP "" ctxStr of
                Left err     -> error (show err)
                Right result -> result
    return (ctx, expr)

fileXP :: Parser [Row]
fileXP = sepEndBy1 ctxAndTermP (many1 (char '\n'))

parseFileXP :: String -> Either ParseError [Row]
parseFileXP = parse (whitespace *> fileXP <* eof) "File"


-- ----------------------------------
-- DEDUCTION



getDed :: HasDedForm a => Row -> [a] ->  Maybe Int
getDed me  = getDed2 (leftSortDed me)


getDed2 ::  HasDedForm a => DedForm -> [a] ->  Maybe Int
getDed2 _ []    = Nothing
getDed2 me (x:xs) =
    if getDedForm x == me
        then Just (1+length xs) 
        else getDed2 me xs



leftDed :: Row -> Row
leftDed (ctx, ta :-> tb) = leftDed (ta : ctx, tb)
leftDed x                = x

leftSortDed :: Row -> DedForm
leftSortDed (ctx, term) = 
    let (ctx', term') = leftDed (ctx, term) in
    (sort $ map show ctx', term')

arePermsEquivalent :: [Term] -> [Term] -> Bool
arePermsEquivalent xs ys = xs' == ys'
    where xs' = sort $ map show xs
          ys' = sort $ map show ys

-- ----------------------------------
-- MODUS PONENS

isModusPonens :: Row -> Row -> Row -> Bool
isModusPonens (c1, t1) (c2, t2) (c3, t3) =
    t1 == (t2 :-> t3) && arePermsEquivalent c1 c2 && arePermsEquivalent c2 c3


-- return indexes

rowGetTerm :: Row -> Term
rowGetTerm (_, term) = term


endsWithMe :: Term -> Row -> Bool
endsWithMe term (ctx, a :-> b) = term == b
endsWithMe _ _ = False




getModusPonens :: HasRow a => [a] -> Row -> Maybe (Int, Int)
getModusPonens [] _ = Nothing
getModusPonens [x] _  = Nothing
getModusPonens rows (ctx, termMe) =
    -- let getRow = accInst
    let indexedRows = zip [0..] rows
        bRows = filter (\(i, x) -> endsWithMe termMe (getRow x)) indexedRows
        pairs = [(i, j) | (i, x) <- bRows, (j, y) <- indexedRows, i /= j]
        in case find (\(i, j) -> isModusPonens (getRow (rows !! i)) (getRow (rows !! j)) (ctx, termMe)) pairs of
        Just (i, j) -> Just (i, j)
        Nothing     -> Nothing


getAxiom :: Term -> Maybe Int
getAxiom term = case term of
    _ | isJust (isa1 term) -> Just 1
    _ | isJust (isa2 term) -> Just 2
    _ | isJust (isa3 term) -> Just 3
    _ | isJust (isa4 term) -> Just 4
    _ | isJust (isa5 term) -> Just 5
    _ | isJust (isa6 term) -> Just 6
    _ | isJust (isa7 term) -> Just 7
    _ | isJust (isa8 term) -> Just 8
    _ | isJust (isa9 term) -> Just 9
    _ | isJust (isa10 term) -> Just 10
      | otherwise -> Nothing


-- --------
-- check for hyp in context

getHyp :: Row -> Maybe Int
getHyp (ctx, term) = elemIndex term ctx

-- ------
-- Final code



firstSol :: [Row] -> [Damn2]
firstSol = foldl' getTree []
-- finSol :: [Row] -> [Damn]
-- finSol = foldl' getTree []


accGetBool :: Damn -> Bool
accGetBool (_, _, x, _) = x


showCtx :: [Term] -> String
showCtx ctx = intercalate "," $ map show ctx


type DedForm = ([String], Term)

type Damn = (Row, String, Bool, DedForm)

class HasStr a where 
    getStr :: a -> String

class HasDedForm a where 
    getDedForm :: a -> DedForm 

class HasRow a where
    getRow :: a -> Row


instance HasStr Damn where
    getStr (_, x, _, _) = x

instance HasRow Damn where
    getRow (row, _, _, _) = row

instance HasDedForm Damn where
    getDedForm (_, _, _, dedForm) = dedForm





-- ----------------------------------
-- AXIOMS



isa1 :: Term -> Maybe (Term, Term)
isa1 (a :-> (b :-> a1)) = if a == a1 then Just (a, b) else Nothing
isa1 _                  = Nothing

isa2 :: Term -> Maybe (Term, Term, Term)
isa2 ((a :-> b) :-> (a1 :-> b1 :-> c) :-> (a2 :-> c1)) =
    if (==) a a1
        && (==) a1 a2
        && (==) b b1
        && (==) c c1
        then Just (a, b, c) else Nothing
isa2 _ = Nothing

isa3 :: Term -> Maybe (Term, Term)
isa3 (a :-> b :-> (a1 `BAnd` b1)) =
    if (==) a a1
        && (==) b b1
        then Just (a, b) else Nothing
isa3 _ = Nothing

isa4 :: Term -> Maybe (Term, Term)
isa4 (BAnd a b :-> a')
    | (==) a a' = Just (a, b)
    | otherwise = Nothing
isa4 _ = Nothing

isa5 :: Term -> Maybe (Term, Term)
isa5 (BAnd a b :-> b')
    | (==) b b' = Just (a, b)
    | otherwise = Nothing
isa5 _ = Nothing

isa6 :: Term -> Maybe (Term, Term)
isa6 (a :-> BOr a' b)
    | (==) a a' = Just (a, b)
    | otherwise = Nothing
isa6 _ = Nothing

isa7 :: Term -> Maybe (Term, Term)
isa7 (b :-> BOr a b')
    | (==) b b' = Just (a, b)
    | otherwise = Nothing
isa7 _ = Nothing

isa8 :: Term -> Maybe (Term, Term, Term)
isa8 ((a :-> c) :-> ((b :-> c') :-> (BOr a' b' :-> c'')))
    | (==) a a' && (==) b b' && (==) c c' && (==) c' c''
        = Just (a, b, c)
    | otherwise = Nothing
isa8 _ = Nothing

isa9 :: Term -> Maybe (Term, Term)
isa9 ((a :-> b) :-> ((a' :->  (b' :-> BNOT)) :-> ( a'' :-> BNOT)))
    | (==) a a' && (==) a' a'' && (==) b b'
        = Just (a, b)
    | otherwise = Nothing
isa9 _ = Nothing

isa10 :: Term -> Maybe Term
isa10 (((a :-> BNOT):->BNOT) :-> a')
    | (==) a a' = Just a
    | otherwise = Nothing
isa10 _ = Nothing




data NodeX a =
    Ax Term Int a
    | Hyp Term a
    | MP Term (NodeX a) (NodeX a) a
    | Ded Term (NodeX a) a
    deriving (Show, Eq, Generic)

nodeGetA :: NodeX a -> a
nodeGetA (Ax _ _ a) = a
nodeGetA (Hyp _ a) = a
nodeGetA (MP _ _ _ a) = a
nodeGetA (Ded _ _ a) = a


nodeGetTerm :: NodeX a -> Term
nodeGetTerm (Ax term _ _) = term
nodeGetTerm (Hyp term _ ) = term
nodeGetTerm (MP term _ _ _) = term
nodeGetTerm (Ded term _ _) = term

type Damn2 = (Row, DedForm, NodeX [Term])

instance HasRow Damn2 where
    getRow (x, _, _) = x

instance HasDedForm Damn2 where
    getDedForm :: Damn2 -> DedForm
    getDedForm (_, x, _) = x

accGetNode :: Damn2 -> NodeX [Term]
accGetNode (_, _, x) = x

getTree :: [Damn2] -> Row -> [Damn2]
getTree acc x =
    let (ctx, term) = x
        n = 1 + length acc
        -- me = "[" ++ show n ++ "] " ++ showCtx ctx ++ "|-" ++ show term
        ax = getAxiom term
        hyp = getHyp x
        ded = getDed x acc
        modus = getModusPonens acc x
        noda = case (ax, hyp, modus, ded) of
            (Just idx, _,  _, _) ->     Ax term idx ctx
            (_, Just i, _, _) ->        Hyp term ctx
            (_, _,  Just (i, j), _) ->  MP term (accGetNode $ acc !! j) (accGetNode $ acc !! i) ctx
            (_, _, _, Just i) ->        Ded term (accGetNode $ acc !! (n-i-1)) ctx
            _ -> error $ "Incorrect" ++ show x
    in (x, leftSortDed x, noda) : acc




-- === СОДЕРЖИМОЕ ФАЙЛА OldX.hs ===




printRow :: Row -> String
printRow (ctx, term) = intercalate "," (map show ctx) ++ "|-" ++ show term

printTRow :: TRow -> String
printTRow (ctx :- term) = intercalate "," (map show ctx) ++ "|-" ++ show term



prettyPrintNode :: Node -> String
prettyPrintNode = go 0
    where
        go indent node = indentStr indent ++ nodeStr node ++ "\n" ++ childrenStr indent node

        nodeStr node = printTRow (nodeGetTRow node) ++ " [" ++ nodeGetTypo node ++ "]"

        childrenStr indent (Eto _ n1 n2) =  go (indent + 1) n1 ++ go (indent + 1) n2
        childrenStr indent (Ito _ n1) =     go indent n1
        childrenStr indent (Iand _ n1 n2) = go (indent + 1) n1 ++ go (indent + 1) n2
        childrenStr indent (Eland _ n1) =   go indent n1
        childrenStr indent (Erand _ n1) =   go indent n1
        childrenStr indent (Ilor _ n1) =    go indent n1
        childrenStr indent (Ilol _ n1) =    go indent n1
        childrenStr indent (Eseq _ n1 n2 n3) = go (indent + 1) n1 ++ go (indent + 1) n2 ++ go (indent + 1) n3
        childrenStr indent (Enotnot _ n1) = go indent n1
        childrenStr _ _ = ""

        indentStr n = replicate n '\t'


printNode :: Int -> Node  -> IO ()
printNode lvl node = do
    let sonNodes = getSonNodes node
    mapM_ (printNode (lvl+1)) sonNodes
    let gh = nodeGetTRow node
    let typo = nodeGetTypo node
    -- putStrLn $  " [" ++ typo ++ "]"
    putStrLn $ "[" ++ show lvl ++ "] " ++ printTRow gh ++ " [" ++ typo ++ "]"
    -- putStrLn $ (replicate lvl '\t') ++ "[" ++ show lvl ++ "] " ++ printTRow gh ++ " [" ++ typo ++ "]"



printRef :: Map String Bool ->  String
printRef m = intercalate ", " $ map (\(a, b) -> a ++ ":=" ++ strb b) $ Data.Map.toList m
    where 
        strb False = "F"
        strb True = "T"


-- === СОДЕРЖИМОЕ ФАЙЛА PrintUtils.hs ===

-- import Lemms (sc1, sc10)



getTermMoves :: Term -> [Term]
getTermMoves (a :-> b) = a : getTermMoves b
getTermMoves _ = []

data Direction = Add | Del


type Moves = [(Term, Direction)]

getDedMoves :: NodeX a -> Moves
getDedMoves (Ded me nodeFrom a) =
    let
        from = nodeGetTerm nodeFrom
        to = me
        fromMoves = (, Add) <$> getTermMoves from
        toMoves =  (, Del) <$> getTermMoves to
        merged = mergeMoves (reverse toMoves) (reverse fromMoves)
    in
        merged
        -- if length merged == 1 
        --     then head merged
        -- else error "too many moves" 
getDedMoves _ = error "getDedMoves: not a Ded"

mergeMoves :: Moves -> Moves -> Moves
mergeMoves [] (y : ys) = mergeMoves [y] ys
mergeMoves xx [] = xx
mergeMoves (x:xs) (y:ys) =
    if fst x == fst y then
        case (snd x, snd y) of
            (Add, Del) -> mergeMoves xs ys
            (Del, Add) -> mergeMoves xs ys
            _ -> mergeMoves (y : x : xs) ys
    else
        mergeMoves (y : x : xs) ys


reform :: NodeX [Term] -> Node

reform (Hyp me ctx) = InContext $ ctx :- me

reform (Ax me axNum ctx) = let
    xx = case axNum of
            1 -> isToSc1 $ fromMaybe (error "reform-1") (isa1 me)  
            2 -> isToSc2 $ fromMaybe (error "reform-2") (isa2 me)
            3 -> isToSc3 $ fromMaybe (error "reform-3") (isa3 me)
            4 -> isToSc4 $ fromMaybe (error "reform-4") (isa4 me)
            5 -> isToSc5 $ fromMaybe (error "reform-5") (isa5 me)
            6 -> isToSc6 $ fromMaybe (error "reform-6") (isa6 me)
            7 -> isToSc7 $ fromMaybe (error "reform-7") (isa7 me)
            8 -> isToSc8 $ fromMaybe (error "reform-8") (isa8 me)
            9 -> isToSc9 $ fromMaybe (error "reform-9") (isa9 me)
            10 -> isToSc10 $ fromMaybe (error "reform-10") (isa10 me)
            _ -> error "reform: not an ax"
    in  xx ctx

reform (MP me nodeFrom1 nodeFrom2 ctx) = 
    -- thowOnInvalidstr ("MP " ++ show me) $
  Eto (ctx :- me) 
  ( reform nodeFrom2) 
  ( reform nodeFrom1)
--   (thowOnInvalidstr ("MP-1->>> " ++ show me) $ reform nodeFrom2) 
--   (thowOnInvalidstr ("MP-2->>> " ++ show me) $ reform nodeFrom1)



reform node@(Ded me nodeFrom ctx) = let  
    yy = getDedMoves node
    (t, dir) = if length yy == 1 then head yy else error "reform: too many moves"
    in
    case dir of
        Add -> -- добваить в контекст т е перенести влево

            moveLeft $ reform nodeFrom
        Del ->  -- вправо
            -- thowOnInvalidstr "Ded Del " $ 
            Ito (ctx :- me) $
             reform nodeFrom

moveRight :: Int -> Node -> Node
moveRight 0 node = node
moveRight x node =  let
    (g :- t) = nodeGetTRow node
    in case g of 
        [] -> error "moveRight: no moves"
        (a:as) ->  moveRight (x - 1) $ Ito (as :- (a :-> t)) node



moveLeft :: Node -> Node
moveLeft node =  let
    (g :- t) = nodeGetTRow node
    (a, b) = getAB t
    fromAdded = addToContext [a] node
    axx = InContext $ (a : g) :- a
    res = Eto ((a : g) :- b) fromAdded axx
    in res

isToSc1 :: (Term, Term) -> [Term] -> Node
isToSc1 (a, b) ctx = sc1 ctx a b

isToSc2 :: (Term, Term, Term) -> [Term] -> Node
isToSc2 (a, b, c) ctx = let
    sg = a : (a :-> b :-> c) : (a :-> b) : ctx
    s1 = InContext $ sg :- a
    s2 = InContext $ sg :- (a :-> b)
    s3 = InContext $ sg :- (a :-> b :-> c)
    s4 = Eto (sg :- (b :-> c)) s3 s1
    s5 = Eto (sg :- b) s2 s1    
    s6 = Eto (sg :- c) s4 s5
    in 
    moveRight 3 s6

isToSc3 :: (Term, Term) -> [Term] -> Node
isToSc3 (a, b) ctx = let
    aAndB = a `BAnd` b
    s1 = InContext $ (a : b : ctx) :- a
    s2 = InContext $ (a : b : ctx) :- b
    s3 = Iand ((a : b : ctx) :- aAndB) s1 s2
    s4 = Ito ((a : ctx) :- (b :-> aAndB)) s3
    s5 = Ito (ctx :- (a :-> (b :-> aAndB))) s4
    in s5

isToSc4 :: (Term, Term) -> [Term] -> Node
isToSc4 (a, b) ctx = memeGenerator ctx (a `BAnd` b) a Eland

isToSc5 :: (Term, Term) -> [Term] -> Node
isToSc5 (a, b) ctx = memeGenerator ctx (a `BAnd` b) b Erand

isToSc6 :: (Term, Term) -> [Term] -> Node
isToSc6 (a, b) ctx = memeGenerator ctx a (a `BOr` b) Ilor

isToSc7 :: (Term, Term) -> [Term] -> Node
isToSc7 (a, b) ctx = memeGenerator ctx b (a `BOr` b) Ilol

isToSc8 :: (Term, Term, Term) -> [Term] -> Node
isToSc8 (a, b, c) ctx = let
    sg = (a `BOr` b) : (a :-> c) : (b :-> c) : ctx
    -- s1 = Ito ((a : sg) :- c) $ 
    s1 = moveLeft $ 
        InContext $ sg :- (a :-> c)
    -- s2 = Ito ((b : sg) :- c) $ 
    s2 = moveLeft $ 
        InContext $ sg :- (b :-> c)
    s3 = InContext $ sg :- (a `BOr` b)
    in
    Ito (ctx :- ((b :-> c) :-> (a :-> c) :-> ((a `BOr` b) :-> c))) $
    Ito (((b :-> c) : ctx) :- ((a :-> c) :-> ((a `BOr` b) :-> c))) $
    Ito (((a :-> c) : (b :-> c) : ctx) :- ((a `BOr` b) :-> c)) $
    Eseq (sg :- c) s1 s2 s3

isToSc9 :: (Term, Term) -> [Term] -> Node
isToSc9 (a, b) = isToSc2 (a, b, BNOT)

isToSc10 :: Term -> [Term] -> Node
isToSc10 a ctx = sc10 ctx a


memeGenerator :: [Term] -> Term -> Term -> (TRow  -> Node  -> Node) -> Node
memeGenerator ctx a b rule =
    Ito (ctx :- (a :-> b)) $
    rule ((a : ctx) :- b) $
    InContext $ (a : ctx) :- a



getSc1 :: [Term] -> Term -> Node
getSc1 ctx term = case isa1 term of
        Just x -> isToSc1 x ctx
        Nothing -> error "getSc1: not a sc1"



sc1 :: [Term] -> Term -> Term -> Node
sc1 g a b =
    Ito (g :- (a :-> b :-> a)) $
    Ito ((a : g) :- (b :-> a)) $
    InContext ((a : b : g) :- a)

    

sc10 :: [Term] -> Term -> Node
sc10 g a = 
    let 
    nna = tnot (tnot a)
    na = tnot a
    s1 = InContext $ ( nna : g) :- nna
    -- s2 = Ito  (( na : nna : g) :- BNOT) s1
    -- s2 = moveRight 1 s1
    s2 = moveLeft s1
    s3 = Enotnot (( nna : g) :- a) s2
    in
    Ito ((g) :- (nna :-> a)) $ s3


-- === СОДЕРЖИМОЕ ФАЙЛА Reform.hs ===

{-# LANGUAGE DataKinds #-}


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


-- === СОДЕРЖИМОЕ ФАЙЛА RTL.hs ===


-- import Data.Set

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






-- === СОДЕРЖИМОЕ ФАЙЛА TrueChecker.hs ===


-- import OldX (main1)

main :: IO ()
-- main = do
--     let xx = isToSc10 (V "A") []
--     printNode 0 $ xx
--     printNode 0 $ thowOnInvalidstr "haha" $ xx
-- main = LemmTests.testAll
-- printSolution (V "A", V "B") (V "A", V "B", V "C")


main = do
    input <- getLine
    let parsed = parseFile input
    case parsed of
        Left err -> error $ show err
        Right term -> do
            -- putStrLn "Parsed term:"
            let mbFalse = searchForFalse term
            case mbFalse of
                Nothing ->
                    printSolution term
                    -- putStrLn "Term is true"
                Just xx ->
                    putStrLn $ "Formula is refutable [" ++ printRef xx ++ "]"
            -- print term
    putStrLn ""


-- === СОДЕРЖИМОЕ ФАЙЛА DMain.hs ===




sekLemmNode :: Node
sekLemmNode =
    let adder = "A->B, !A->B|-"
        p1 = (adder ++) <$>
            ["A->B",
            "(A -> B) -> (!B -> (A -> B))",
            "!B -> (A -> B)",
            "!B -> (A -> !B)",
            "(A -> B) -> ((A -> !B) -> !A)",
            "((A -> B) -> ((A -> !B) -> !A)) -> (!B -> ((A -> B) -> ((A -> !B) -> !A)))",
            "!B -> ((A -> B) -> ((A -> !B) -> !A))",
            "(!B -> (A -> B)) -> ((!B -> ((A -> B) -> ((A -> !B) -> !A))) -> (!B -> ((A -> !B) -> !A)))",
            "(!B -> ((A -> B) -> ((A -> !B) -> !A))) -> (!B -> ((A -> !B) -> !A))",
            "!B -> ((A -> !B) -> !A)",
            "(!B -> (A -> !B)) -> ((!B -> ((A -> !B) -> !A)) -> (!B -> !A))",
            "(!B -> ((A -> !B) -> !A)) -> (!B -> !A)",
            "!B -> !A",
            "!A->B",
            "(!A -> B) -> (!B -> (!A -> B))",
            "!B -> (!A -> B)",
            "!B -> (!A -> !B)",
            "(!A -> B) -> ((!A -> !B) -> !!A)",
            "((!A -> B) -> ((!A -> !B) -> !!A)) -> (!B -> ((!A -> B) -> ((!A -> !B) -> !!A)))",
            "!B -> ((!A -> B) -> ((!A -> !B) -> !!A))",
            "(!B -> (!A -> B)) -> ((!B -> ((!A -> B) -> ((!A -> !B) -> !!A))) -> (!B -> ((!A -> !B) -> !!A)))",
            "(!B -> ((!A -> B) -> ((!A -> !B) -> !!A))) -> (!B -> ((!A -> !B) -> !!A))",
            "!B -> ((!A -> !B) -> !!A)",
            "(!B -> (!A -> !B)) -> ((!B -> ((!A -> !B) -> !!A)) -> (!B -> !!A))",
            "(!B -> ((!A -> !B) -> !!A)) -> (!B -> !!A)",
            "!B -> !!A",
            "(!B -> !A) -> (!B -> !!A) -> !!B",
            "(!B -> !!A) -> !!B",
            "!!B",
            "!!B -> B",
            "B"]
        p2 = p1 ++ ["(A->B) |- (!A->B) -> B",
                    "|-(A->B) -> (!A->B) -> B"]
    in justPeremena p2

sekLemm :: Term -> Term -> Node
sekLemm a b =
    -- thowOnInvalidstr "sekLemm " $
    insertInProof a b $
    sekLemmNode




justPeremena :: [String] ->  Node
justPeremena ls =
    let
    parsed = map (parse ctxAndTermP "") ls
    solutions = firstSol $ rights parsed
    errors = lefts parsed
    twoTree = if null errors then reform $ accGetNode $ head solutions
    else
        error $ "First parse error: " ++ show (head errors)
    -- res = insertInProof a b 
    in twoTree


peremena :: Term -> Term -> [String] ->  Node
peremena a b ls =
    let
    parsed = map (parse ctxAndTermP "") ls
    solutions = firstSol $ rights parsed
    errors = lefts parsed
    twoTree = if null errors then reform $ accGetNode $ head solutions
    else
        error $ "First parse error: " ++ show (head errors)
    res = insertInProof a b twoTree
    in res


getLemm :: Map String Bool -> Term -> Node
getLemm estimap (a :-> b) =
    if b == BNOT then
        if evalterm estimap a then
            lemmTo10 a b
            else
                InContext $ [tnot a, tnot BNOT] :- tnot a -- !a :- !a
    else

        case (evalterm estimap a, evalterm estimap b) of
        (False, False) -> lemmTo00 a b
        (True, False)  -> lemmTo10 a b
        (False, True)  -> lemmTo01 a b
        (True, True)   -> lemmTo11 a b


getLemm estimap (a `BAnd` b) = case (evalterm estimap a, evalterm estimap b) of
    (False, False) -> insertInProof a b lemmAnd00Node
    (True, False)  -> insertInProof a b lemmAnd10Node
    (False, True)  -> insertInProof a b lemmAnd01Node
    (True, True)   -> insertInProof a b lemmAnd11Node

getLemm estimap (a `BOr` b) = case (evalterm estimap a, evalterm estimap b) of
    (False, False) -> insertInProof a b lemmOr00Node
    (True, False)  -> insertInProof a b lemmOr10Node
    (False, True)  -> insertInProof a b lemmOr01Node
    (True, True)   -> insertInProof a b lemmOr11Node


getLemm _  _= undefined


lemmTo01 :: Term -> Term -> Node
lemmTo01 a b = insertInProof a b lemmTo01Node

lemmTo11 :: Term -> Term -> Node
lemmTo11 a b = insertInProof a b lemmTo11Node

lemmTo00 :: Term -> Term -> Node
lemmTo00 a b = insertInProof a b lemmTo00Node

lemmOr11Node :: Node
lemmOr11Node = justPeremena $
    let xx = "A,B |- " in
          (xx ++) <$> [ "A", "A -> A | B", "A | B"]


lemmOr01Node :: Node
lemmOr01Node = justPeremena $
    let xx = "!A,B |- " in
          (xx ++) <$> ["B", "B -> A | B", "A | B"]

lemmOr10Node :: Node
lemmOr10Node = justPeremena $
    let xx = "A,!B |- " in
          (xx ++) <$> [ "A", "A -> A | B", "A | B"]

lemmOr00Node :: Node
lemmOr00Node = justPeremena $
    [ "|- !A -> !B -> !(A|B)",
      "!A |- !B -> !(A|B)",
      "!A, !B |- !(A|B)"
    ]

lemmAnd11Node :: Node
lemmAnd11Node = justPeremena $
    let xx = "A,B |- " in
          (xx ++) <$>
          [ "A", "B",
            "A -> B -> A & B",
            "B -> A & B",
            "A & B"]

lemmAnd01Node ::  Node
lemmAnd01Node = 
    -- Eto ([V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> BNOT)) (Eto ([V "A" :-> BNOT,V "B"] :- ((BAnd (V "A") (V "B") :-> (V "A" :-> BNOT)) :-> (BAnd (V "A") (V "B") :-> BNOT))) (Ito ([V "A" :-> BNOT,V "B"] :- ((BAnd (V "A") (V "B") :-> V "A") :-> ((BAnd (V "A") (V "B") :-> (V "A" :-> BNOT)) :-> (BAnd (V "A") (V "B") :-> BNOT)))) (Ito ([BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- ((BAnd (V "A") (V "B") :-> (V "A" :-> BNOT)) :-> (BAnd (V "A") (V "B") :-> BNOT))) (Ito ([BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> BNOT)) (Eto ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- BNOT) (Eto ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- (V "A" :-> BNOT)) (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> (V "A" :-> BNOT)))) (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- BAnd (V "A") (V "B")))) (Eto ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- V "A") (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> V "A"))) (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "A" :-> BNOT),BAnd (V "A") (V "B") :-> V "A",V "A" :-> BNOT,V "B"] :- BAnd (V "A") (V "B")))))))) (Ito ([V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> V "A")) (Eland ([BAnd (V "A") (V "B"),V "A" :-> BNOT,V "B"] :- V "A") (InContext ([BAnd (V "A") (V "B"),V "A" :-> BNOT,V "B"] :- BAnd (V "A") (V "B")))))) (Eto ([V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> (V "A" :-> BNOT))) (Ito ([V "A" :-> BNOT,V "B"] :- ((V "A" :-> BNOT) :-> (BAnd (V "A") (V "B") :-> (V "A" :-> BNOT)))) (Ito ([V "A" :-> BNOT,V "A" :-> BNOT,V "B"] :- (BAnd (V "A") (V "B") :-> (V "A" :-> BNOT))) (InContext ([V "A" :-> BNOT,BAnd (V "A") (V "B"),V "A" :-> BNOT,V "B"] :- (V "A" :-> BNOT))))) (InContext ([V "A" :-> BNOT,V "B"] :- (V "A" :-> BNOT))))
    justPeremena $
    let xx = "!A,B |- " in
          (xx ++) <$>
    [ "!A", "!A -> A & B -> !A",
      "A & B -> !A",
      "A",
      "A & B -> A",
      "(A & B -> A) -> (A & B -> !A) -> !(A & B)",
      "(A & B -> !A) -> !(A & B)",
      "!(A & B)"
      ]

lemmAnd10Node ::  Node
lemmAnd10Node = 
    -- Eto ([V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> BNOT)) (Eto ([V "A",V "B" :-> BNOT] :- ((BAnd (V "A") (V "B") :-> (V "B" :-> BNOT)) :-> (BAnd (V "A") (V "B") :-> BNOT))) (Ito ([V "A",V "B" :-> BNOT] :- ((BAnd (V "A") (V "B") :-> V "B") :-> ((BAnd (V "A") (V "B") :-> (V "B" :-> BNOT)) :-> (BAnd (V "A") (V "B") :-> BNOT)))) (Ito ([BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- ((BAnd (V "A") (V "B") :-> (V "B" :-> BNOT)) :-> (BAnd (V "A") (V "B") :-> BNOT))) (Ito ([BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> BNOT)) (Eto ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- BNOT) (Eto ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- (V "B" :-> BNOT)) (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> (V "B" :-> BNOT)))) (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- BAnd (V "A") (V "B")))) (Eto ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- V "B") (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> V "B"))) (InContext ([BAnd (V "A") (V "B"),BAnd (V "A") (V "B") :-> (V "B" :-> BNOT),BAnd (V "A") (V "B") :-> V "B",V "A",V "B" :-> BNOT] :- BAnd (V "A") (V "B")))))))) (Ito ([V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> V "B")) (Erand ([BAnd (V "A") (V "B"),V "A",V "B" :-> BNOT] :- V "B") (InContext ([BAnd (V "A") (V "B"),V "A",V "B" :-> BNOT] :- BAnd (V "A") (V "B")))))) (Eto ([V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> (V "B" :-> BNOT))) (Ito ([V "A",V "B" :-> BNOT] :- ((V "B" :-> BNOT) :-> (BAnd (V "A") (V "B") :-> (V "B" :-> BNOT)))) (Ito ([V "B" :-> BNOT,V "A",V "B" :-> BNOT] :- (BAnd (V "A") (V "B") :-> (V "B" :-> BNOT))) (InContext ([V "B" :-> BNOT,BAnd (V "A") (V "B"),V "A",V "B" :-> BNOT] :- (V "B" :-> BNOT))))) (InContext ([V "A",V "B" :-> BNOT] :- (V "B" :-> BNOT))))

    justPeremena $
    let xx = "A,!B |- " in
          (xx ++) <$>
    [ "!B","!B -> A & B -> !B",
      "A & B -> !B",
      "A & B -> B",
      "(A & B -> B) -> (A & B -> !B) -> !(A & B)",
      "(A & B -> !B) -> !(A & B)",
      "!(A & B)"
      ]

lemmAnd00Node :: Node
lemmAnd00Node = justPeremena $
    let xx = "!A,!B |- " in
          (xx ++) <$>
    [ "!A", "!A -> A & B -> !A",
      "A & B -> !A",
      "A & B -> A",
      "(A & B -> A) -> (A & B -> !A) -> !(A & B)",
      "(A & B -> !A) -> !(A & B)",
      "!(A & B)"
      ]

lemmTo01Node :: Node
lemmTo01Node  =
    justPeremena $
    let xx = "!A,B |- " in
          (xx ++) <$>
          [
           "B",
          "B -> A -> B",
          "(A -> B)"
    ]

lemmTo11Node :: Node
lemmTo11Node =
    justPeremena $
    let xx = "A,B |- " in
          (xx ++) <$>
          [
           "B",
           "B -> A -> B",
           "(A -> B)"
    ]

lemmTo00Node :: Node
lemmTo00Node =
    let
    xx = "!A, !B, A |- "
    res =   ((xx ++) <$> [ "A",
            "A -> !(A -> B) -> A",
            "!(A -> B) -> A",
            "!A",
            "!A -> !(A -> B) -> !A",
            "!(A -> B) -> !A",
            "(!(A -> B) -> A) -> (!(A -> B) -> !A) -> !!(A -> B)",
            "(!(A -> B) -> !A) -> !!(A -> B)",
            "!!(A -> B)",
            "!!(A -> B) -> (A -> B)",
            "A -> B",
            "B"])
                ++
             ["!A, !B|- A -> B"]

    in justPeremena res

lemmTo10 :: Term -> Term -> Node
lemmTo10 a b =
    let
    gs = [a:->b, a, b :-> BNOT]
    s1 = InContext (gs :- a)
    s2 = InContext (gs :- (a :-> b))
    s3 = InContext (gs :- (b :-> BNOT))
    s4 = Eto (gs :- b) s2 s1
    s5 = Eto (gs :- BNOT) s3 s4
    res = moveRight 1 s5
    in
    res


-- === СОДЕРЖИМОЕ ФАЙЛА Lemms.hs ===

-- {-# LANGUAGE DeriveGeneric #-}


whitespace :: Parser ()
whitespace = skipMany (oneOf " \t\r")
-- whitespace = void space
-- whitespace = space

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: String -> Parser String
symbol = lexeme . string

variable :: Parser Term
variable = lexeme $ do
    first <- letter
    rest <- many (letter <|> digit <|> char '\'')
    return $ V (first:rest)

notP :: Parser Term
notP = lexeme $ do
    void $ string "_|_"
    return BNOT

varOrNotP :: Parser Term
varOrNotP = variable <|> notP

termP :: Parser Term
termP = buildExpressionParser table term
  where
    table = [ [Infix  (BAnd <$ symbol "&") AssocLeft]
            , [Infix  (BOr <$ symbol "|") AssocLeft]
            , [Infix  ((:->) <$ symbol "->") AssocRight]
            ]

    term = parens termP <|> varOrNotP
    parens = between (symbol "(") (symbol ")")


parseTerm :: String -> Either ParseError Term
parseTerm = parse (whitespace *> termP <* eof) "Expression"

contextP :: Parser [Term]
contextP = sepBy termP (symbol ",")

contextStrP :: Parser String
contextStrP = manyTill anyChar (try (string "|-"))

parseFile :: String -> Either ParseError Term
parseFile = parse (whitespace *> termP <* eof) "File"




-- === СОДЕРЖИМОЕ ФАЙЛА Parser.hs ===




-- import Prelude hiding (lookup)
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

        gi = (\x -> if Map.lookup x estimap == Just True then V x else tnot (V x)) <$> getVarsUniq t

        proofHelper :: Term -> Node

        proofHelper (V a) =  InContext (gi :- modulate estimap (V a))

        proofHelper BNOT = -- gi :- BNOT :-> BNOT
            
            Ito (gi :- (BNOT :-> BNOT)) $
            InContext ((BNOT : gi) :- BNOT)

        proofHelper alpha =
            let
                (a, b) = getAB alpha
                aProof = proofHelper a
                bProof = proofHelper b
                lol =  getLemm estimap alpha -- одна из 14ти лемм
            in
            kwazar aProof bProof lol







kwazar :: Node -> Node -> Node -> Node
kwazar na nb nlol =
    Eto (gi :- c) step1 nb
    where
        step1 = Eto (gi :- (b :-> c)) step2 na
        step2 = Ito (gi :- (a :-> b :-> c))
            $ Ito ((a : gi) :- (b :-> c))
            $ lolWith
        lolWith = addToContext gi nlol -- [=одна из 14ти лемм ] с добавлением gi в контекст по всему дереву
        (gi :- a) = nodeGetTRow na
        (_ :- b) = nodeGetTRow nb
        (_ :- c) = nodeGetTRow nlol




-- === СОДЕРЖИМОЕ ФАЙЛА ProofThings.hs ===

-- {-# LANGUAGE DataKinds #-}

-- {-# LANGUAGE DeriveGeneric #-}



-- import           GHC.Generics        (Generic)

data Term =
      V String
    | Term :-> Term
    | BAnd Term Term
    | BOr Term Term
    | BNOT
    deriving (Eq, Ord)
    -- deriving (Eq, Ord, Show)
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

tnot :: Term -> Term
tnot a = a :-> BNOT


-- data Node x where
--     Eto :: Node (ctx, a :-> b) -> Node (ctx, a) -> Node (ctx, Term)

getAB :: Term -> (Term, Term)
getAB (a :-> b) = (a, b)
getAB (a `BAnd` b) = (a, b)
getAB (a `BOr` b) = (a, b)
getAB _ = error "getAB: not a -> b"


-- === СОДЕРЖИМОЕ ФАЙЛА Term.hs ===

