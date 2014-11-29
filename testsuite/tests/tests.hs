module Main where

-- import Haggressive.Main
import Test.HUnit
import Data.Vector as V
import Data.Map as M
import qualified Hag as H

-- Lists
list1 :: [String]
list1 = [ "ich", "bin", "aber", "ich", "bin", "nicht" ]

-- Vectors
vec1 :: V.Vector String
vec1 = V.fromList list1


-- Maps
map1, map2 :: M.Map String Int
map1 = M.fromList [ ("ich", 2)
                  , ("bin", 2)
                  , ("aber", 1)
                  , ("nicht", 1)
                  ]
map2 = M.fromList [ ("hab", 2)
                  , ("bin", 2)
                  , ("aber", 1)
                  , ("nicht", 2)
                  ]


-- Define tests
testFrequency1, testFrequency2 :: Test
testFrequency1 = H.frequency vec1 ~?= map1
testFrequency2 = H.frequency vec1 ~?= map2


-- Run tests
tests :: IO Counts
tests = runTestTT (TestList [ testFrequency1
                            , testFrequency2
                           ])

-- main :: IO()
main = do
  tests
