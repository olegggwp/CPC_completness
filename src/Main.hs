{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict #-}

module Main where
import           Control.Applicative ((<|>))
import           Text.Parsec         hiding ((<|>))
import           Text.Parsec.Expr
import           Text.Parsec.String  (Parser)
import           Control.Monad       (void)
import           GHC.Generics        (Generic)
import           Data.Either         (rights)
import           Data.List
import           Data.Maybe          (isJust)
-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"

main :: IO ()
main = do
    input <- getContents
    let ls = lines input
    let parsed = map (parse contextAndTermP "") ls
    let parsed' = rights parsed
    let solutions = firstSol parsed'
    let firstTree = accGetNode $ head solutions
    putStrLn $ prettyPrintNode firstTree
    let rededTree = deductionRemake firstTree []
    putStrLn $ prettyPrintNode rededTree



prettyPrintNode :: Node a -> String
prettyPrintNode = go 0
    where
        go indent (Ax term idx _) = replicate indent '\t' ++ "Ax " ++ show term ++ " " ++ show idx ++ "\n"
        go indent (Hyp term _) = replicate indent '\t' ++ "Hyp " ++ show term ++ "\n"
        go indent (MP term n1 n2 a) =
                replicate indent '\t' ++ "MP " ++ show term ++ "\n" ++
                go (indent + 1) n1 ++
                go (indent + 1) n2
        go indent (Ded term n a) =
                replicate indent '\t' ++ "Ded " ++ show term ++ " moves: " ++ show (getDedMoves (Ded term n a)) ++ "\n" ++
                go (indent + 1) n

    -- mapM_ (putStrLn . getStr) (reverse solutions)
    -- print "OK"
    -- mapM_ print parsed


data Term =
      V String
    | Term :-> Term
    | BAnd Term Term
    | BOr Term Term
    | BNot Term
    deriving (Eq, Generic)
infixr 2 :->
infixl 3 `BOr`
infixl 4 `BAnd`

instance Show Term where
    show (V name)   = name
    show (a :-> b)  = "(" ++ show a ++ "->" ++ show b ++ ")"
    show (BAnd a b) = "(" ++ show a ++ "&" ++ show b ++ ")"
    show (BOr a b)  = "(" ++ show a ++ "|" ++ show b ++ ")"
    show (BNot a)   = "!(" ++ show a ++ ")"


-- ----------------------------------



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

termP :: Parser Term
termP = buildExpressionParser table term
  where
    table = [ [Prefix (BNot <$ symbol "!")]
            , [Infix  (BAnd <$ symbol "&") AssocLeft]
            , [Infix  (BOr <$ symbol "|") AssocLeft]
            , [Infix  ((:->) <$ symbol "->") AssocRight]
            ]

    term = prefixNot <|> parens termP <|> variable
    parens = between (symbol "(") (symbol ")")
    prefixNot = do
        nots <- many1 (symbol "!")
        t <- term
        return $ foldr (const BNot) t nots

parseTerm :: String -> Either ParseError Term
parseTerm = parse (whitespace *> termP <* eof) "Expression"

contextP :: Parser [Term]
contextP = sepBy termP (symbol ",")

contextStrP :: Parser String
contextStrP = manyTill anyChar (try (string "|-"))

contextAndTermP :: Parser Row
contextAndTermP = do
    _ <- many space
    ctxStr <- contextStrP
    _ <- many space
    expr <- termP
    let ctx = case parse contextP "" ctxStr of
                Left err     -> error (show err)
                Right result -> result
    return (ctx, expr)

fileP :: Parser [Row]
fileP = sepEndBy1 contextAndTermP (many1 (char '\n'))

parseFile :: String -> Either ParseError [Row]
parseFile = parse (whitespace *> fileP <* eof) "File"


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

-- isDed :: Row -> Row -> Bool
-- isDed (c1, t1) (c2, t2) =
--     let (c1', t1') = leftDed (c1, t1)
--         (c2', t2') = leftDed (c2, t2)
--     in arePermsEquivalent c1' c2' && t1' == t2'


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

-- accInst :: Damn -> Row
-- accInst (x, _, _, _) = x

accGetBool :: Damn -> Bool
accGetBool (_, _, x, _) = x

-- accGetStr :: Damn -> String
-- accGetStr (_, x, _, _) = x

-- accGetDedForm :: Damn -> DedForm
-- accGetDedForm (_, _, _, x) = x

showCtx :: [Term] -> String
showCtx ctx = intercalate "," $ map show ctx

type Row = ([Term], Term)

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



-- getLineNig :: [Damn] -> Row -> [Damn]
-- getLineNig acc x=
--     let (ctx, term) = x
--         n = 1 + length acc
--         me = "[" ++ show n ++ "] " ++ showCtx ctx ++ "|-" ++ show term
--         ax = getAxiom term
--         hyp = getHyp x
--         ded = getDed x acc
--         modus = getModusPonens acc x
--         (linee, flag) =
--             -- debug
--             -- (show $ length acc, True)



--             case (ax, hyp, ded, modus) of
--             (Just idx, _,  _, _) ->
--                 (" [Ax. sch. " ++ show idx ++ "]" ,
--                 True)

--             (_, Just i, _, _) ->
--                 (" [Hyp. " ++ show (i+1) ++ "]", True)

--             (_, _, Just i, _) ->
--                 let
--                     ii = n-i-1
--                     add = if accGetBool (acc !! ii)
--                         then "" else "; from Incorrect"
--                 in
--                 (" [Ded. " ++ show (i) ++ add ++ "]", True)

--             (_, _,  _, Just (i, j)) ->
--                 let
--                     add = if accGetBool (acc !! i) && accGetBool (acc !! j)
--                         then "" else "; from Incorrect"
--                 in
--                 (" [M.P. " ++ show (n-j-1) ++ ", " ++ show (n-i-1) ++ add ++ "]", True)



--             _ -> (" [Incorrect]", False)
    -- in (x, me ++ linee, flag, leftSortDed x) : acc



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
isa9 ((a :-> b) :-> ((a' :-> BNot b') :-> BNot a''))
    | (==) a a' && (==) a' a'' && (==) b b'
        = Just (a, b)
    | otherwise = Nothing
isa9 _ = Nothing

isa10 :: Term -> Maybe Term
isa10 (BNot (BNot a) :-> a')
    | (==) a a' = Just a
    | otherwise = Nothing
isa10 _ = Nothing














-- -------
-- a for metadata 
-- like from what line of original proof

data Node a =
    Ax Term Int a
    | Hyp Term a
    | MP Term (Node a) (Node a) a
    | Ded Term (Node a) a
    deriving (Show, Eq, Generic)

nodeGetA :: Node a -> a
nodeGetA (Ax _ _ a) = a
nodeGetA (Hyp _ a) = a
nodeGetA (MP _ _ _ a) = a
nodeGetA (Ded _ _ a) = a


nodeGetTerm :: Node a -> Term
nodeGetTerm (Ax term _ _) = term
nodeGetTerm (Hyp term _ ) = term
nodeGetTerm (MP term _ _ _) = term
nodeGetTerm (Ded term _ _) = term

type Damn2 = (Row, DedForm, Node ())

instance HasRow Damn2 where
    getRow (x, _, _) = x

instance HasDedForm Damn2 where
    getDedForm (_, x, _) = x

accGetNode :: Damn2 -> Node ()
accGetNode (_, _, x) = x

getTree :: [Damn2] -> Row -> [Damn2]
getTree acc x=
    let (ctx, term) = x
        n = 1 + length acc
        me = "[" ++ show n ++ "] " ++ showCtx ctx ++ "|-" ++ show term
        ax = getAxiom term
        hyp = getHyp x
        ded = getDed x acc
        modus = getModusPonens acc x
        noda = case (ax, hyp, ded, modus) of
            (Just idx, _,  _, _) ->
                Ax term idx ()
                -- (" [Ax. sch. " ++ show idx ++ "]" ,True)

            (_, Just i, _, _) ->
                Hyp term ()
                -- (" [Hyp. " ++ show (i+1) ++ "]", True)

            (_, _, Just i, _) ->
                let
                    ii = n-i-1
                in
                Ded term (accGetNode $ acc !! ii) ()
                -- (" [Ded. " ++ show i ++ "]", True)

            (_, _,  _, Just (i, j)) ->
                MP term (accGetNode $ acc !! i) (accGetNode $ acc !! j) ()
                -- (" [M.P. " ++ show (n-j-1) ++ ", " ++ show (n-i-1) ++ "]", True)
            _ -> error "Incorrect"
    in (x, leftSortDed x, noda) : acc




-- ----------------------------------

-- добавить в контекст\убрать из контекста
data Todo = Add | Del
    deriving (Show)

type Moves = [(Term, Todo)]

-- moves пеердающийся дальше может меняться  только при прохождени Ded


-- head moves это первое что нужно сделать


-- data Node2 a = 
--     Ax2 Term Int a
--     | Hyp2 Term a
--     | MP2 Term (Node2 a) (Node2 a) a
    -- | Ded Term (Node a) a



getOnlyRight :: Term -> Term
getOnlyRight (a :-> b) = b
getOnlyRight _  = error "THIS IS VERY BAD"



nodeAdder :: Node a -> Term -> Moves -> Node a
nodeAdder node al xs = let
        a = nodeGetA node
        hypp = Hyp al a
        likeax = deductionRemake node xs
        noda = MP (getOnlyRight (nodeGetTerm node)) hypp likeax a
    in deductionRemake noda xs

deductionRemake :: Node a -> Moves -> Node a

deductionRemake (Ax ax idx a) [] = Ax ax idx a
deductionRemake nodeMe@(Ax ax idx a) (move:xs) =
    let
        noda = case move of
            (al, Add) -> 
                let
                    hypp = Hyp al a
                    likeax = deductionRemake nodeMe xs
                    res = MP (getOnlyRight ax) hypp likeax a
                in
                    res
            -- Ax ax idx a
            (alpha, Del) -> MP (alpha :-> ax) (Ax ax idx a) (Ax (ax :-> alpha :-> ax) 1 a) a
    in
    deductionRemake noda xs


deductionRemake (Hyp hyp a) [] = Hyp hyp a
deductionRemake nodeMe@(Hyp hyp a) (move:xs) =
    let
        noda = case move of
            (al, Add) ->
                let
                    hypp = Hyp al a
                    likeax = deductionRemake nodeMe xs
                    res = MP (getOnlyRight hyp) hypp likeax a
                in
                    res
 
                -- (Hyp hyp a)
            (alpha, Del) ->
                if alpha == hyp
                    then
                        let
                            n02 = Ax (hyp :-> hyp :-> hyp) 1 a
                            n04 = Ax ((hyp :-> hyp :-> hyp) :-> (hyp :-> (hyp :-> hyp) :-> hyp) :-> (hyp :-> hyp)) 2 a
                            n06 = MP ((hyp :-> (hyp :-> hyp) :-> hyp) :-> (hyp :-> hyp)) n02 n04 a
                            n08 = Ax (hyp :-> (hyp :-> hyp) :-> hyp) 1 a
                            n1 = MP (hyp :-> hyp) n08 n06 a
                        in n1
                    else
                        MP (alpha :-> hyp) (Hyp hyp a) (Ax (hyp :-> alpha :-> hyp) 1 a) a

    in
    deductionRemake noda xs

deductionRemake (MP term n1 n2 a) [] =
    let
        n1' = deductionRemake n1 []
        n2' = deductionRemake n2 []
    in
    MP term n1' n2' a



deductionRemake nodeMe@(MP me n1 n2 a) (move : xs)  =
    let
        nj' = deductionRemake n1 (move : xs)
        nk' = deductionRemake n2 (move : xs)
        newThisNode = case move of
            (al, Add) -> 
                let
                    hypp = Hyp al a
                    likeax = deductionRemake nodeMe xs
                    res = MP (getOnlyRight me) hypp likeax a
                in
                    res
            (al, Del) ->
                let
                    bj = nodeGetTerm n1
                    bn1 = me
                    n03 = Ax ((al :-> bj) :-> (al :-> bj :-> bn1) :-> (al :-> bn1)) 2 a
                    n06 = MP ((al :-> bj :-> bn1) :-> (al :-> bn1)) nj' n03 a
                    nfinal = MP (al :-> bn1) nk' n06 a
                in
                    nfinal
    in
        deductionRemake newThisNode xs



deductionRemake (Ded me nodeFrom a) moves =
    let
        from = nodeGetTerm nodeFrom
        to = me
        fromMoves = map (, Add) $ getTermMoves from
        toMoves = map (, Del) $ getTermMoves to
        newMoves = mergeMoves moves $ reverse $ mergeMoves fromMoves toMoves
    in
        deductionRemake nodeFrom newMoves


getTermMoves :: Term -> [Term]
getTermMoves (a :-> b) = a : getTermMoves b
getTermMoves _ = []

getDedMoves :: Node a -> Moves
getDedMoves (Ded me nodeFrom a) =
    let
        from = nodeGetTerm nodeFrom
        to = me
        fromMoves = map (, Add) $ getTermMoves from
        toMoves = map (, Del) $ getTermMoves to
    in
        mergeMoves fromMoves toMoves
getDedMoves _ = []

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
