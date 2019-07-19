module Ast where

data Term 
    = Abs String Term
    | App Term Term
    | Var String
    deriving (Show,Eq)