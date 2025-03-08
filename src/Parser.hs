module Parser where
-- {-# LANGUAGE DeriveGeneric #-}
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


