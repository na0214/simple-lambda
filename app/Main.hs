{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text hiding (take)
import ParserLam
import Ast

main :: IO ()
main = print $ (parse parseTerm "$x.x") `feed` ""
