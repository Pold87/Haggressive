{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Haggressive.Main
       (
         main
       ) where

import qualified Data.Vector as V
import qualified Data.Map as M

data Tweet = Tweet { category :: String
                   , user :: String
                   , date :: String
                   , time :: String
                   , message :: String
                   } deriving (Show, Eq, Ord)


countItem :: (Ord a) => M.Map a Int -> a -> M.Map a Int
countItem m e = M.insertWith (+) e 1 m

frequency :: Ord a => V.Vector a -> M.Map a Int
frequency = V.foldl' countItem M.empty


main :: IO ()
main = 
  print "hey"
