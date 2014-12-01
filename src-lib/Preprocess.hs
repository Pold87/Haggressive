{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Preprocess
       (preprocess)
       where

import qualified Data.Text as T

preprocess :: T.Text -> T.Text
preprocess txt = T.cons '\"' $ T.snoc escaped '\"'
  where escaped = T.concatMap escaper txt

escaper :: Char -> T.Text
escaper c
  | c == '\t' = "\"\t\""
  | c == '\n' = "\"\n\""
  | c == '\"' = "\"\""
  | otherwise = T.singleton c
