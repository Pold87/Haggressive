import Haggressive.Main
import Test.HUnit

-- Lists
list1 :: [String]
list1 = [ "ich", "bin", "aber", "ich", "bin", "nicht" ]

-- Vectors
vec1 :: V.Vector String
vec1 = V.fromList list1


-- Maps
map1 :: M.Map String Int
map1 = M.fromList [ ("ich", 2)
                  , ("bin", 2)
                  , ("aber", 1)
                  , ("nicht", 1)
                  ]


-- Define tests
testFrequency1 :: Test
testFrequency1 = frequency vec1 ~?= map1

-- Run tests
tests :: IO Counts
tests = runTest (TestList [ testFrequency1
                         --,
                           ])

main :: IO()
main = do
  tests
