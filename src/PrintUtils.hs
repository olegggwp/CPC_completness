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


printRow :: Row -> String
printRow (ctx, term) = intercalate "," (map show ctx) ++ "|-" ++ show term


