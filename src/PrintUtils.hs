module PrintUtils where
import Term

import           Control.Applicative ((<|>))
import           Text.Parsec         hiding ((<|>))
import           Text.Parsec.Expr
import           Text.Parsec.String  (Parser)
import           Control.Monad       (void)
import           GHC.Generics        (Generic)
import           Data.Either         (rights)
import           Data.List
import           Data.Maybe          (isJust)
import Debug.Trace
import RTL


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


printNode :: Node -> Int -> IO ()
printNode node lvl = do
    let sonNodes = getSonNodes node
    mapM_ (\x -> printNode x (lvl+1)) sonNodes
    let gh = nodeGetTRow node
    let typo = nodeGetTypo node
    putStrLn $ "[" ++ show lvl ++ "] " ++ printTRow gh ++ " [" ++ show typo ++ "]"

