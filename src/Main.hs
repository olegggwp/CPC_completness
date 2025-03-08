-- -- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- module Main where
-- import           Control.Applicative ((<|>))
-- import           Text.Parsec         hiding ((<|>))
-- import           Text.Parsec.Expr
-- import           Text.Parsec.String  (Parser)
-- import           Control.Monad       (void)
-- import           GHC.Generics        (Generic)
-- import           Data.Either         (rights)
-- import           Data.List
-- import           Data.Maybe          (isJust)
-- import Debug.Trace

-- main :: IO ()
-- main = do
--     input <- getContents
--     let ls = lines input
--     let parsed = map (parse contextAndTermP "") ls
--     let parsed' = rights parsed
--     let solutions = firstSol parsed'
--     let firstTree = accGetNode $ head solutions
--     -- let firstTree = (Ax (V "A" :-> V "B" :-> (V "A" `BAnd` V "B")) 11 ())
--     let firstTree' = toBool firstTree
--     -- let rededTree = deductionRemake firstTree' [(V "A", Add), (V "B", Add)]

--     let rededTree = deductionRemake firstTree' []
--     -- putStrLn $ prettyPrintNode rededTree

--     -- putStrLn $ printRow $ last parsed'

--     putStrLn $ printRow $ getRow $ head solutions

--     let was = map (rowGetTerm . getRow) solutions


--     putStrLn $ printNode rededTree was

-- -- debug:
--     -- putStrLn $ prettyPrintNode firstTree'
--     -- putStrLn $ prettyPrintNode rededTree




-- toBool :: Node a -> Node Bool
-- toBool (Ax term idx _) = Ax term idx True
-- toBool (Hyp term _) = Hyp term True
-- toBool (MP term n1 n2 _) = MP term (toBool n1) (toBool n2) True
-- toBool (Ded term n _) = Ded term (toBool n) True


-- printRow :: Row -> String
-- printRow (ctx, term) = intercalate "," (map show ctx) ++ "|-" ++ show term


-- prettyPrintNode :: Show a => Node a -> String
-- prettyPrintNode = go 0
--     where
--         go indent node = indentStr indent ++ nodeStr node ++ "\n" ++ childrenStr indent node

--         nodeStr node = nodeTypeStr node ++ show (nodeGetTerm node) ++ showA (nodeGetA node)

--         nodeTypeStr (Ax _ _ _) = "Ax "
--         nodeTypeStr (Hyp _ _) = "Hyp "
--         nodeTypeStr (MP _ _ _ _) = "MP "
--         nodeTypeStr (Ded term n a) = "Ded " ++ " moves: " ++ show (getDedMoves (Ded term n a)) ++ ";  "

--         childrenStr indent (MP _ n1 n2 _) = go (indent + 1) n1 ++ go (indent + 1) n2
--         childrenStr indent (Ded _ n _) = go (indent + 1) n
--         childrenStr _ _ = ""

--         indentStr n = replicate n '\t'

--         showA a = ""
--         -- showA a = " [a : " ++ show a ++ "]"


-- printNode :: Node Bool -> [Term] -> String
-- printNode node was = let
--     body = case node of
--         Ax term _ _    -> show term
--         Hyp term _     -> show term
--         MP _ n1 n2 _   -> printNode n1 was ++ printNode n2 was ++ show (nodeGetTerm node)
--         Ded _ n _      -> printNode n was ++ show (nodeGetTerm node)
--     in body ++ printAboutOrig node was


-- lolBool :: Bool -> String
-- lolBool a = if a then " [from Original proof]" else ""

-- searchForTerm :: Term -> [Term] -> Bool
-- searchForTerm = elem

-- isFromOrig :: Node Bool -> [Term] -> Bool
-- isFromOrig node was =
--     searchForTerm (nodeGetTerm node) was
--     -- nodeGetA node || searchForTerm (nodeGetTerm node) was

-- printAboutOrig :: Node Bool -> [Term] -> String
-- printAboutOrig node was =
--     if isFromOrig node was then " [from Original proof]\n" else "\n"

-- data Term =
--       V String
--     | Term :-> Term
--     | BAnd Term Term
--     | BOr Term Term
--     | BNot Term
--     deriving (Eq, Generic)
-- infixr 2 :->
-- infixl 3 `BOr`
-- infixl 4 `BAnd`

-- instance Show Term where
--     show (V name)   = name
--     show (a :-> b)  = "(" ++ show a ++ "->" ++ show b ++ ")"
--     show (BAnd a b) = "(" ++ show a ++ "&" ++ show b ++ ")"
--     show (BOr a b)  = "(" ++ show a ++ "|" ++ show b ++ ")"
--     show (BNot a)   = "!(" ++ show a ++ ")"


-- -- ----------------------------------



-- whitespace :: Parser ()
-- whitespace = skipMany (oneOf " \t\r")
-- -- whitespace = void space
-- -- whitespace = space

-- lexeme :: Parser a -> Parser a
-- lexeme p = p <* whitespace

-- symbol :: String -> Parser String
-- symbol = lexeme . string

-- variable :: Parser Term
-- variable = lexeme $ do
--     first <- letter
--     rest <- many (letter <|> digit <|> char '\'')
--     return $ V (first:rest)

-- termP :: Parser Term
-- termP = buildExpressionParser table term
--   where
--     table = [ [Prefix (BNot <$ symbol "!")]
--             , [Infix  (BAnd <$ symbol "&") AssocLeft]
--             , [Infix  (BOr <$ symbol "|") AssocLeft]
--             , [Infix  ((:->) <$ symbol "->") AssocRight]
--             ]

--     term = prefixNot <|> parens termP <|> variable
--     parens = between (symbol "(") (symbol ")")
--     prefixNot = do
--         nots <- many1 (symbol "!")
--         t <- term
--         return $ foldr (const BNot) t nots

-- parseTerm :: String -> Either ParseError Term
-- parseTerm = parse (whitespace *> termP <* eof) "Expression"

-- contextP :: Parser [Term]
-- contextP = sepBy termP (symbol ",")

-- contextStrP :: Parser String
-- contextStrP = manyTill anyChar (try (string "|-"))

-- contextAndTermP :: Parser Row
-- contextAndTermP = do
--     _ <- many space
--     ctxStr <- contextStrP
--     _ <- many space
--     expr <- termP
--     let ctx = case parse contextP "" ctxStr of
--                 Left err     -> error (show err)
--                 Right result -> result
--     return (ctx, expr)

-- fileP :: Parser [Row]
-- fileP = sepEndBy1 contextAndTermP (many1 (char '\n'))

-- parseFile :: String -> Either ParseError [Row]
-- parseFile = parse (whitespace *> fileP <* eof) "File"


-- -- ----------------------------------
-- -- DEDUCTION



-- getDed :: HasDedForm a => Row -> [a] ->  Maybe Int
-- getDed me  = getDed2 (leftSortDed me)


-- getDed2 ::  HasDedForm a => DedForm -> [a] ->  Maybe Int
-- getDed2 _ []    = Nothing
-- getDed2 me (x:xs) =
--     if getDedForm x == me
--         then Just (1+length xs)
--         else getDed2 me xs

-- -- isDed :: Row -> Row -> Bool
-- -- isDed (c1, t1) (c2, t2) =
-- --     let (c1', t1') = leftDed (c1, t1)
-- --         (c2', t2') = leftDed (c2, t2)
-- --     in arePermsEquivalent c1' c2' && t1' == t2'


-- leftDed :: Row -> Row
-- leftDed (ctx, ta :-> tb) = leftDed (ta : ctx, tb)
-- leftDed x                = x

-- leftSortDed :: Row -> DedForm
-- leftSortDed (ctx, term) =
--     let (ctx', term') = leftDed (ctx, term) in
--     (sort $ map show ctx', term')

-- arePermsEquivalent :: [Term] -> [Term] -> Bool
-- arePermsEquivalent xs ys = xs' == ys'
--     where xs' = sort $ map show xs
--           ys' = sort $ map show ys

-- -- ----------------------------------
-- -- MODUS PONENS

-- isModusPonens :: Row -> Row -> Row -> Bool
-- isModusPonens (c1, t1) (c2, t2) (c3, t3) =
--     t1 == (t2 :-> t3) && arePermsEquivalent c1 c2 && arePermsEquivalent c2 c3


-- -- return indexes

-- rowGetTerm :: Row -> Term
-- rowGetTerm (_, term) = term


-- endsWithMe :: Term -> Row -> Bool
-- endsWithMe term (ctx, a :-> b) = term == b
-- endsWithMe _ _ = False




-- getModusPonens :: HasRow a => [a] -> Row -> Maybe (Int, Int)
-- getModusPonens [] _ = Nothing
-- getModusPonens [x] _  = Nothing
-- getModusPonens rows (ctx, termMe) =
--     -- let getRow = accInst
--     let indexedRows = zip [0..] rows
--         bRows = filter (\(i, x) -> endsWithMe termMe (getRow x)) indexedRows
--         pairs = [(i, j) | (i, x) <- bRows, (j, y) <- indexedRows, i /= j]
--         in case find (\(i, j) -> isModusPonens (getRow (rows !! i)) (getRow (rows !! j)) (ctx, termMe)) pairs of
--         Just (i, j) -> Just (i, j)
--         Nothing     -> Nothing


-- getAxiom :: Term -> Maybe Int
-- getAxiom term = case term of
--     _ | isJust (isa1 term) -> Just 1
--     _ | isJust (isa2 term) -> Just 2
--     _ | isJust (isa3 term) -> Just 3
--     _ | isJust (isa4 term) -> Just 4
--     _ | isJust (isa5 term) -> Just 5
--     _ | isJust (isa6 term) -> Just 6
--     _ | isJust (isa7 term) -> Just 7
--     _ | isJust (isa8 term) -> Just 8
--     _ | isJust (isa9 term) -> Just 9
--     _ | isJust (isa10 term) -> Just 10
--       | otherwise -> Nothing



-- getHyp :: Row -> Maybe Int
-- getHyp (ctx, term) = elemIndex term ctx

-- firstSol :: [Row] -> [Damn2]
-- firstSol = foldl' getTree []

-- accGetBool :: Damn -> Bool
-- accGetBool (_, _, x, _) = x

-- showCtx :: [Term] -> String
-- showCtx ctx = intercalate "," $ map show ctx

-- type Row = ([Term], Term)

-- type DedForm = ([String], Term)

-- type Damn = (Row, String, Bool, DedForm)

-- class HasStr a where
--     getStr :: a -> String

-- class HasDedForm a where
--     getDedForm :: a -> DedForm

-- class HasRow a where
--     getRow :: a -> Row


-- instance HasStr Damn where
--     getStr (_, x, _, _) = x

-- instance HasRow Damn where
--     getRow (row, _, _, _) = row

-- instance HasDedForm Damn where
--     getDedForm (_, _, _, dedForm) = dedForm


-- -- ----------------------------------
-- -- AXIOMS



-- isa1 :: Term -> Maybe (Term, Term)
-- isa1 (a :-> (b :-> a1)) = if a == a1 then Just (a, b) else Nothing
-- isa1 _                  = Nothing

-- isa2 :: Term -> Maybe (Term, Term, Term)
-- isa2 ((a :-> b) :-> (a1 :-> b1 :-> c) :-> (a2 :-> c1)) =
--     if (==) a a1
--         && (==) a1 a2
--         && (==) b b1
--         && (==) c c1
--         then Just (a, b, c) else Nothing
-- isa2 _ = Nothing

-- isa3 :: Term -> Maybe (Term, Term)
-- isa3 (a :-> b :-> (a1 `BAnd` b1)) =
--     if (==) a a1
--         && (==) b b1
--         then Just (a, b) else Nothing
-- isa3 _ = Nothing

-- isa4 :: Term -> Maybe (Term, Term)
-- isa4 (BAnd a b :-> a')
--     | (==) a a' = Just (a, b)
--     | otherwise = Nothing
-- isa4 _ = Nothing

-- isa5 :: Term -> Maybe (Term, Term)
-- isa5 (BAnd a b :-> b')
--     | (==) b b' = Just (a, b)
--     | otherwise = Nothing
-- isa5 _ = Nothing

-- isa6 :: Term -> Maybe (Term, Term)
-- isa6 (a :-> BOr a' b)
--     | (==) a a' = Just (a, b)
--     | otherwise = Nothing
-- isa6 _ = Nothing

-- isa7 :: Term -> Maybe (Term, Term)
-- isa7 (b :-> BOr a b')
--     | (==) b b' = Just (a, b)
--     | otherwise = Nothing
-- isa7 _ = Nothing

-- isa8 :: Term -> Maybe (Term, Term, Term)
-- isa8 ((a :-> c) :-> ((b :-> c') :-> (BOr a' b' :-> c'')))
--     | (==) a a' && (==) b b' && (==) c c' && (==) c' c''
--         = Just (a, b, c)
--     | otherwise = Nothing
-- isa8 _ = Nothing

-- isa9 :: Term -> Maybe (Term, Term)
-- isa9 ((a :-> b) :-> ((a' :-> BNot b') :-> BNot a''))
--     | (==) a a' && (==) a' a'' && (==) b b'
--         = Just (a, b)
--     | otherwise = Nothing
-- isa9 _ = Nothing

-- isa10 :: Term -> Maybe Term
-- isa10 (BNot (BNot a) :-> a')
--     | (==) a a' = Just a
--     | otherwise = Nothing
-- isa10 _ = Nothing










-- -- -------
-- -- a for metadata 
-- -- like from what line of original proof

-- data Node a =
--     Ax Term Int a
--     | Hyp Term a
--     | MP Term (Node a) (Node a) a
--     | Ded Term (Node a) a
--     deriving (Show, Eq, Generic)

-- nodeGetA :: Node a -> a
-- nodeGetA (Ax _ _ a) = a
-- nodeGetA (Hyp _ a) = a
-- nodeGetA (MP _ _ _ a) = a
-- nodeGetA (Ded _ _ a) = a


-- nodeGetTerm :: Node a -> Term
-- nodeGetTerm (Ax term _ _) = term
-- nodeGetTerm (Hyp term _ ) = term
-- nodeGetTerm (MP term _ _ _) = term
-- nodeGetTerm (Ded term _ _) = term

-- type Damn2 = (Row, DedForm, Node ())

-- instance HasRow Damn2 where
--     getRow (x, _, _) = x

-- instance HasDedForm Damn2 where
--     getDedForm (_, x, _) = x

-- accGetNode :: Damn2 -> Node ()
-- accGetNode (_, _, x) = x

-- getTree :: [Damn2] -> Row -> [Damn2]
-- getTree acc x =
--     let (ctx, term) = x
--         n = 1 + length acc
--         -- me = "[" ++ show n ++ "] " ++ showCtx ctx ++ "|-" ++ show term
--         ax = getAxiom term
--         hyp = getHyp x
--         ded = getDed x acc
--         modus = getModusPonens acc x
--         noda = case (ax, hyp, modus, ded) of
--             (Just idx, _,  _, _) -> Ax term idx ()
--             (_, Just i, _, _) -> Hyp term ()
--             (_, _,  Just (i, j), _) -> MP term (accGetNode $ acc !! j) (accGetNode $ acc !! i) ()
--             (_, _, _, Just i) -> Ded term (accGetNode $ acc !! (n-i-1)) ()
--             _ -> error $ "Incorrect" ++ show x
--     in (x, leftSortDed x, noda) : acc




-- -- ----------------------------------

-- -- добавить в контекст\убрать из контекста
-- data Todo = Add | Del
--     deriving (Show)

-- type Moves = [(Term, Todo)]


-- nodeSetFalse :: Node a -> Node Bool
-- nodeSetFalse (Ax term idx _) = Ax term idx False
-- nodeSetFalse (Hyp term _) = Hyp term False
-- nodeSetFalse (MP term n1 n2 _) = MP term (nodeSetFalse n1) (nodeSetFalse n2) False
-- nodeSetFalse (Ded term n _) = Ded term (nodeSetFalse n) False


-- nodeSetTrue :: Node Bool -> Node Bool
-- nodeSetTrue (Ax term idx _) = Ax term idx True
-- nodeSetTrue (Hyp term _) = Hyp term True
-- nodeSetTrue (MP term n1 n2 _) = MP term n1 n2 True
-- nodeSetTrue (Ded term n _) = Ded term n True

-- getOnlyRight :: Term -> Term
-- getOnlyRight (a :-> b) = b
-- getOnlyRight x  = error $ "THIS IS VERY BAD: " ++ show x


-- nodeAdder :: Node Bool -> Term -> Node Bool
-- nodeAdder node al = let
--     -- a = nodeGetA node
--     hypp = Hyp al False
--     -- likeax = node
--     in MP (getOnlyRight (nodeGetTerm node)) hypp node False

-- deductionRemake :: Node Bool -> Moves -> Node Bool
-- deductionRemake node@(Ax ax idx a) [] = node
-- deductionRemake nodeMe@(Ax ax idx a) (move:xs) = let
--     noda = case move of
--         (al, Add) -> nodeAdder nodeMe al
--         (alpha, Del) -> MP (alpha :-> ax) (Ax ax idx a) (Ax (ax :-> alpha :-> ax) 1 False) False
--     in deductionRemake noda xs


-- deductionRemake node@(Hyp hyp a) [] = node
-- deductionRemake nodeMe@(Hyp hyp a) (move:xs) =
--     let
--         noda = case move of
--             (al, Add) -> nodeAdder nodeMe al
--             (alpha, Del) ->
--                 if alpha == hyp then
--                     let
--                         n02 = Ax (hyp :-> hyp :-> hyp) 1 False
--                         n04 = Ax ((hyp :-> hyp :-> hyp) :-> (hyp :-> (hyp :-> hyp) :-> hyp) :-> (hyp :-> hyp)) 2 False
--                         n06 = MP ((hyp :-> (hyp :-> hyp) :-> hyp) :-> (hyp :-> hyp)) n02 n04 False
--                         n08 = Ax (hyp :-> (hyp :-> hyp) :-> hyp) 1 False
--                         n1 = MP (hyp :-> hyp) n08 n06 a
--                     in n1
--                 else
--                     MP (alpha :-> hyp) (Hyp hyp a) (Ax (hyp :-> alpha :-> hyp) 1 False) False
--     in
--     deductionRemake noda xs

-- deductionRemake (MP term n1 n2 a) [] =
--     let
--         n1' = deductionRemake n1 []
--         n2' = deductionRemake n2 []
--     in
--     MP term n1' n2' a



-- deductionRemake nodeMe@(MP me n1 n2 a) (move : xs)  =
--     let
--         newThisNode = case move of
--             (al, Add) -> nodeAdder nodeMe al
--             (al, Del) ->
--                 let
--                     nj' = deductionRemake n1 (move : [])
--                     nk' = deductionRemake n2 (move : [])
--                     bj = nodeGetTerm n1
--                     _ = trace ("BJ is : " ++ (show bj) ++ " ===== \n" ) ()
--                     bn1 = me
--                     n03 = Ax ((al :-> bj) :-> (al :-> bj :-> bn1) :-> (al :-> bn1)) 2 False
--                     n06 = MP ((al :-> bj :-> bn1) :-> (al :-> bn1)) nj' n03 False
--                     nfinal = MP (al :-> bn1) nk' n06 False
--                 in
--                     nfinal
--     in
--         deductionRemake newThisNode xs



-- deductionRemake (Ded me nodeFrom a) moves =
--     let
--         from = nodeGetTerm nodeFrom
--         to = me
--         fromMoves = map (, Add) $ getTermMoves from
--         toMoves = map (, Del) $ getTermMoves to
--         _ = trace ("!!!FROM" ++ (show fromMoves) ++ " ===== \n") ()
--         _ = trace ("!!!TO" ++ (show toMoves) ++ " ===== \n") ()
--         -- _ = trace ("!!!MERGED" ++ (show (mergeMoves fromMoves toMoves)) ++ " ===== \n") ()
--         m1 = mergeMoves (reverse toMoves) (reverse fromMoves)

--         newMoves = mergeMoves (moves) $ reverse m1
--         -- newMoves = mergeMoves (reverse moves) $ reverse m1
--         _ = trace ("RED" ++ (show newMoves) ++ " ===== \n") ()
--         lol = deductionRemake nodeFrom newMoves
--         lol2 = nodeSetTrue lol
--     in
--         lol2

-- getTermMoves :: Term -> [Term]
-- getTermMoves (a :-> b) = a : getTermMoves b
-- getTermMoves _ = []

-- getDedMoves :: Node a -> Moves
-- getDedMoves (Ded me nodeFrom a) =
--     let
--         from = nodeGetTerm nodeFrom
--         to = me
--         fromMoves =map (, Add) $ getTermMoves from
--         toMoves = map (, Del) $ getTermMoves to
--         _ = trace ("&&&& FROM" ++ (show fromMoves) ++ " ===== \n") ()
--         _ = trace ("&&&& TO" ++ (show toMoves) ++ " ===== \n") ()

--     in
--         -- fromMoves
--         mergeMoves (reverse toMoves) (reverse fromMoves)
-- getDedMoves _ = []

-- mergeMoves :: Moves -> Moves -> Moves
-- mergeMoves [] (y : ys) = mergeMoves [y] ys
-- mergeMoves xx [] = xx
-- mergeMoves (x:xs) (y:ys) =
--     if fst x == fst y then
--         case (snd x, snd y) of
--             (Add, Del) -> mergeMoves xs ys
--             (Del, Add) -> mergeMoves xs ys
--             _ -> mergeMoves (y : x : xs) ys
--     else
--         mergeMoves (y : x : xs) ys
