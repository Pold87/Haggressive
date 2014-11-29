module Tweethelpers
  (mkCrossValScheme
, stopWords
, filterByLabel
, Tweet(..)) where

import           Control.Applicative ((<$>), (<*>), (<|>))
import           Control.Monad       (mzero)
import           Data.Csv
import qualified Data.Vector         as V


stopWordsFile :: FilePath
stopWordsFile = "../dutch-stop-words.txt"

stopWords :: [String]
stopWords = ["aan","af","al","alles","als","altijd","andere","ben","bij","daar","dan","dat","de","der","deze","die","dit","doch","doen","door","dus","een","eens","en","er","ge","geen","geweest","haar","had","heb","hebben","heeft","hem","het","hier","hij ","hoe","hun","iemand","iets","ik","in","is","ja","je ","kan","kon","kunnen","maar","me","meer","men","met","mij","mijn","moet","na","naar","niet","niets","nog","nu","of","om","omdat","ons","ook","op","over","reeds","te","tegen","toch","toen","tot","u","uit","uw","van","veel","voor","want","waren","was","wat","we","wel","werd","wezen","wie","wij","wil","worden","zal","ze","zei","zelf","zich","zij","zijn","zo","zonder","zou"]

-- Parsing Record to Tweet
instance FromRecord Tweet where
  parseRecord v
         | V.length v == 5 = Tweet <$>
                             v.! 0 <*>
                             v.! 1 <*>
                             v.! 2 <*>
                             v.! 3 <*>
                             v.! 4
         | otherwise = mzero



-- A Tweet consists of a category, a user, a date, a time, and a message
data Tweet = Tweet { tLabel   :: String
                   , tUser    :: String
                   , tDate    :: String
                   , tTime    :: String
                   , tMessage :: String
                   } deriving (Show, Eq, Ord)


-- Filter Tweets by label
filterByLabel :: V.Vector Tweet -> String -> V.Vector Tweet
filterByLabel tweets label = V.filter (\t -> tLabel t == label) tweets


-- Make a cross-validation scheme from a list of vectors
mkCrossValScheme :: (Eq a) =>  [V.Vector a] -> [(V.Vector a,V.Vector a)]
mkCrossValScheme xs = map (leaveOneOut xs) xs

-- Create pair of a list of vectors and a vector that specifies which
-- vector should be left out
leaveOneOut :: (Eq a) => [V.Vector a] -> V.Vector a -> (V.Vector a,V.Vector a)
leaveOneOut all test = (test, V.concat $ filter (/= test) all)
