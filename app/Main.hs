{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text hiding (take)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import ParserLam
import Ast

main :: IO ()
main = 
    do
        t <- TIO.getLine
        print $ (parse parseTerm t) `feed` ""
