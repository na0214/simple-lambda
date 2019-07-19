module ParserLam (parseTerm) where

import qualified Ast as AST
import qualified Data.Text as T
import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import Control.Monad

parseTerm :: Parser AST.Term
parseTerm = parseApp <|> parseSimpleExpr <|> parseP

parseSimpleExpr :: Parser AST.Term
parseSimpleExpr = parseAbs <|> parseVar

parseP :: Parser AST.Term
parseP =
    do
        _ <- char '('
        _ <- skipSpace
        t <- parseTerm
        _ <- skipSpace
        _ <- char ')'
        _ <- skipSpace
        return t


parseAbs :: Parser AST.Term
parseAbs =
    do
        AST.Abs 
            <$ char '$' 
            <* skipSpace
            <*> many1 letter
            <* skipSpace
            <* char '.' 
            <* skipSpace
            <*> parseTerm
            <* skipSpace

parseVar :: Parser AST.Term
parseVar =
    do
        AST.Var
            <$> many1 letter
            <* skipSpace

parseApp :: Parser AST.Term
parseApp =  
        do
            s <- parseSimpleExpr
            l <- many1 parseSimpleExpr
            return $ makeApp $ reverse (s:l)
    where
        makeApp (x:[]) = x
        makeApp (x:xs) = AST.App (makeApp xs) x