module My where

import           Data.Map               as M
import           Data.Tuple.Curry
import           Data.Vector            as V
import qualified Distribution.TestSuite as TS
import qualified Hag                    as H
import qualified Test.HUnit             as HU



-- Little helpers

-- parseTweet(s) were in Hag but it turned out that they are not necessary
-- there, since you can parse the CSV ad hoc, i.e., during reading the file

parseTweets :: V.Vector (String, String, String, String, String) -> V.Vector H.Tweet
parseTweets = V.map $ uncurryN H.Tweet

parseTweet :: (String, String, String, String, String) -> H.Tweet
parseTweet (a,b,c,d,e) = H.Tweet a b c d e


-- Lists
list1, list2, list3 :: [String]
list1 = [ "ich", "bin", "aber", "ich", "bin", "nicht" ]
list2 = ["hey", "du", "hey", "du"]
list3 = []

-- Vectors
vec1, vec2, vec3 :: V.Vector String
vec1 = V.fromList list1
vec2 = V.fromList list2
vec3 = V.fromList list3


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

-- Tuples
tup1 = ("aggressive", "Monday", "Time", "User", "Whatup?? I like this sh******t!!")
tup2 = ("a", "b", "c", "d", "e")
tup3 = ("foo", "baa", "haa", "mu", "sa")

tup4 = ("1", "2", "3", "4", "5")
tup5 = ("z", "y", "x", "a", "Oh my goodness \"!§$%&/()=?")

tup6 = ("b", "b", "b", "b", "Text Mining is cool")
tup7 = ("a", "a", "a", "a", "Text Mining sucks")

-- Tweets
[tweet1, tweet2, tweet3, tweet4, tweet5, tweet6, tweet7] = Prelude.map parseTweet [tup1, tup2, tup3, tup4, tup5, tup6, tup7]

-- Define tests

-- frequency --
testFrequency1, testFrequency3 :: HU.Test
testFrequency1 = H.frequency vec1 HU.~?= map1
testFrequency2 = H.frequency vec2 HU.~?= map2
testFrequency3 = H.frequency vec3 HU.~?= M.empty



vecTweets1, vecTweets2, vecTweets3 :: V.Vector H.Tweet
vecTweets1 = parseTweets $ V.fromList [tup1, tup2, tup3]
vecTweets2 = parseTweets $ V.fromList [tup4, tup5]
vecTweets3 = parseTweets $ V.fromList [tup2, tup5]



tweet1_m :: H.Tweet
tweet1_m = H.Tweet "aggressive"
                   "Monday"
                   "Time"
                   "User"
                   "Whatup?? I like this sh******t!!"




-- parseTweet
parseTweetTest1 :: HU.Test
parseTweetTest1 = parseTweet tup1 HU.~?= tweet1_m


mkCrossValSchemeTest0, mkCrossValSchemeTest1, mkCrossValSchemeTest2 :: HU.Test
mkCrossValSchemeTest0= H.mkCrossValScheme ([] :: [V.Vector H.Tweet]) HU.~?= []
mkCrossValSchemeTest1 =
  H.mkCrossValScheme [vecTweets1, vecTweets2]  HU.~?= [(vecTweets1, vecTweets2)
                                                     , (vecTweets2, vecTweets1)
                                                       ]
mkCrossValSchemeTest2 =
  H.mkCrossValScheme [vecTweets1, vecTweets2, vecTweets3] HU.~?= [(vecTweets1, vecTweets2 V.++ vecTweets3)
                                                                , (vecTweets2, vecTweets1 V.++ vecTweets3)
                                                                , (vecTweets3, vecTweets1 V.++ vecTweets2)]



featureIntersectionTest1 :: HU.Test
featureIntersectionTest1 = H.featureIntersection tweet6 (V.fromList [tweet6]) HU.~?=
                    V.fromList [M.fromList [("text", 2)
                                           , ("mining", 2)
                                           , ("is", 2)
                                           , ("cool", 2)]]

featureDistanceTest1 :: HU.Test
featureDistanceTest1 = H.featureDistance tweet6 (V.fromList [tweet6, tweet7]) HU.~?=
                    V.fromList [4, 2]



createDictionaryTest1 :: HU.Test
createDictionaryTest1 = H.createDictionary (V.fromList [tweet6, tweet7]) HU.~?=
  M.fromList [("text", 2)
            , ("mining", 2)
            , ("is", 1)
            , ("sucks", 1)
            , ("cool", 1)]

hunitTests = HU.TestList [testFrequency1
                        , testFrequency3
                        -- , parseTweetTest1
                        , mkCrossValSchemeTest0
                        , mkCrossValSchemeTest1
                        , mkCrossValSchemeTest2
                        , createDictionaryTest1
                        , featureIntersectionTest1
                        , featureDistanceTest1
                        ]

runHUnitTests :: HU.Test -> IO TS.Progress
runHUnitTests tests = do
   (HU.Counts cases tried errors failures) <- HU.runTestTT tests
   return $ if errors > 0
      then TS.Finished $ TS.Error "There were errors in the HUnit tests"
      else if failures > 0
         then TS.Finished $ TS.Fail "There were failures in the HUnit tests"
         else TS.Finished TS.Pass

tests :: IO [TS.Test]
tests = return [ TS.Test hunit ]
  where
    hunit = TS.TestInstance
        { TS.run = runHUnitTests hunitTests
        , TS.name = "HUnit Test Cases"
        , TS.tags = ["hunit"]
        , TS.options = []
        , TS.setOption = \_ _ -> Right hunit
        }

main :: IO [TS.Test]
main = tests
