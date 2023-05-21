module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Expr
import Data.Char

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    return x = Parser $ \input -> Just (x, input)
    (Parser p) >>= f = Parser $ \input -> do
        (x, input') <- p input
        (y, input'') <- parse (f x) input'
        return $ (y, input'')

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> 
        p1 input <|> p2 input


--- type declaration over ---

charP :: Char -> Parser Char
charP x = Parser f
    where 
        f (y:ys)
            | y == x = Just(y, ys)
            | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

wsP :: Parser String
wsP = spanP isSpace

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \x -> Just (span f x)

variableP :: Parser String
variableP = spanP isAlphaNum

nonNullP :: (Char -> Bool) -> Parser String
nonNullP w = Parser f
    where 
        f (y:ys)
            | w y = Just ("", y:ys)
            | otherwise = Nothing
        f [] = Just ("", "")

optionalP :: Parser Char
optionalP = Parser $ \input -> Just (' ', input)

variable :: Parser Expr
variable = do
    nonNullP (/= ' ')
    nonNullP isAlpha
    v <- variableP
    return $ Variable v

combine :: Expr -> Expr -> Expr
combine e_1 e_2 = Application e_1 e_2

function :: Parser Expr
function = do
    wsP
    -- spanP (== '(')
    (charP '(') <|> optionalP
    stringP "\\"
    v <- variableP
    charP '.'
    b <- expr
    (charP ')') <|> optionalP
    return $ Function v b
    -- (\_ var _ body -> Function var body) <$> (wsP *> stringP "\\") <*> variableP <*> charP '.' <*> expr  

application :: Parser Expr
application = do
    wsP
    (charP '(') <|> optionalP
    e_1 <- variable <|> function
    charP ' '
    e_2 <- variable <|> function <|> application
    e_3 <- (plusapplication $ Application e_1 e_2) <|> return (Application e_1 e_2)
    (charP ')') <|> optionalP
    return $ e_3

plusapplication :: Expr -> Parser Expr
plusapplication e = do
    charP ' '
    e_1 <- function <|> variable
    e_2 <- (plusapplication $ Application e e_1) <|> return (Application e e_1)
    return $ e_2


expr :: Parser Expr
expr = function <|> application <|> variable


-- TODO 2.1. parse a expression
parse_expr :: String -> Expr
parse_expr input = case parse expr input of
    Just (e, _) -> e
    Nothing -> Variable "x"

-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code = undefined
