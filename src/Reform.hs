module Reform where
import Term
import RTL
import OldX
-- import Lemms (sc1, sc10)
import Data.Maybe (fromMaybe)



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