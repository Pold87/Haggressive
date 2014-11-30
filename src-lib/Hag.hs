{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}


{-|
Module      : Hag
Description : Classify Tweets (aggressive vs. non_aggressive) and evaluate
              classification performance.
License     : None
Maintainer  : Volker Strobel (volker.strobel87@gmail.com)
Stability   : experimental
Portability : None

This module is the main interface for Tweet classification.

-}
module Hag
( module Hag -- Should be changed after debugging
 ) where

import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.Csv
import           Data.Either
import           Data.List
import qualified Data.Map             as M
import qualified Data.PSQueue         as PS
import qualified Data.Text            as T
import           Data.Text.Encoding
import qualified Data.Text.IO         as TI
import qualified Data.Vector          as V
import           Debug.Trace
import           Helpers
import           NLP.Tokenize
import           Preprocess
import qualified System.Directory     as S
import           System.Environment
import           Tweets


-- | Features are represented by a 'M.Map', where the keys are
-- 'String's (e.g., the words in the message of a Tweet) and the
-- values are 'Float's (e.g., the number of occurrence of a word)
type FeatureMap = M.Map String Float

-- |
-- =IO and Parsing

-- |'parseCsv' parses a 'T.Text' input for fields in CSV format and
-- returns a 'Vector' of 'Tweet's
parseCsv :: T.Text -> Either String (V.Vector Tweet)
parseCsv text = decodeWith
    defaultDecodeOptions {decDelimiter = fromIntegral $ ord '\t' }
    NoHeader
    (L.fromStrict $
     encodeUtf8 text)

-- |Get directory contents of 'FilePath'. A better variant is at:
getFiles :: FilePath -> IO [FilePath]
getFiles dir = S.getDirectoryContents dir
               >>= return . map (dir ++) . filter (`notElem` [".", ".."])

-- |Extract features (the bag of unigrams) for one Tweet.
-- Thereby, the Tweet will be (in order of application):
-- * tokenized
-- * converted to a 'V.Vector'
-- * 'String's will be converted to lowercase
-- * 'String's that are element of 'stopWords' are removed
-- * Empty 'String's will be removed
extractFeatures :: Tweet -> FeatureMap
extractFeatures tweet = bagOfUnigrams
  where
    preprocess = V.filter (`notElem` ["USER", "RT"])
                 . V.fromList
                 . tokenize
                 . tMessage
    bagOfUnigrams = frequency
                 . V.filter (/= "")
                 . V.filter (`notElem` stopWords)
                 . V.map (map toLower) -- filter isAlpha .
                 . preprocess
                 $ tweet

-- |Calculate the 'frequency' of items in a 'V.Vector' and return them
-- in a 'M.Map'.
frequency :: V.Vector String -> FeatureMap
frequency = V.foldl' countItem M.empty

-- |Insert an item into a 'M.Map'. Default value is 1 if the item is
-- not existing. If the item is already existing, its frequency will
-- be increased by 1.
countItem :: M.Map String Float -> String -> FeatureMap
countItem myMap item = M.insertWith (+) item 1 myMap

-- |Calculate the number of exclamation and question marks and the
-- amount of capital letters
getMarks :: String -> FeatureMap
getMarks str = M.fromList $ exclamationList ++ questionList
  where
    exclamationMarks = fromIntegral $ length $ filter (== '!') str -- TODO general case
    questionMarks = fromIntegral $ length $ filter (== '?') str
    exclamationList = if exclamationMarks > 3
                      then [("!", exclamationMarks / 4)]
                      else []
    questionList = if questionMarks > 3
                      then [("!", questionMarks / 4)]
                      else []


-- | Calculate the amount of capitalized words
getCaps :: V.Vector String -> FeatureMap
getCaps vecStr = M.fromList $ if sum > 3 then [("$caps", sum / 4)] else []
  where amount = V.map (length . filter isUpper) vecStr
        sum = fromIntegral $ V.foldl (+) 0 amount :: Float

-- |Take a 'M.Map', consisting of
-- key: 'Tweet'
-- value: 'FeatureMap'
-- and one Tweet and create a new 'M.Map' with the added features
-- from the 'Tweet'
insertInMap :: M.Map Tweet FeatureMap
               -> Tweet
               -> M.Map Tweet FeatureMap
insertInMap oldMap tweet = M.insert tweet val oldMap
  where val = extractFeatures tweet

-- | Compare two vectors of 'Tweet's, the first is the test vector,
-- the second the train vector and return the all neighbors for each
-- 'Tweet'. 'grandDict' is a 'M.Map', where each entry consits of a
-- 'Tweet' and its features
getNeighbors :: (V.Vector Tweet, V.Vector Tweet)
            -> V.Vector (Tweet, [PS.Binding Tweet Float])
getNeighbors (v1,v2) =  V.map (featureIntersection dictionary) v1
  where dictionary = V.foldl insertInMap M.empty v2 :: M.Map Tweet FeatureMap

-- | Take a dictionary and a 'Tweet' and return a pair of this 'Tweet'
-- and all its nearest neighbors
featureIntersection :: M.Map Tweet FeatureMap
                              -> Tweet
                              ->  (Tweet, [PS.Binding Tweet Float])
featureIntersection tweetMap tweet = (tweet, mini)
  where
    mini = PS.atMost 0
           (PS.fromList
           $ M.elems
           $ M.mapWithKey (mergeTweetFeatures cosineDistance tweet) tweetMap)


-- |Take a distance function, 'Tweet' 1, 'Tweet' 2 and a dictionary as
-- 'FeatureMap' and create a 'PS.Binding' between 'Tweet' 2 and the
-- distance from this 'Tweet' to the other 'Tweet'.
mergeTweetFeatures :: (FeatureMap -> FeatureMap -> Float)
                      -> Tweet
                      -> Tweet
                      -> FeatureMap
                      -> PS.Binding Tweet Float
mergeTweetFeatures distF t1 t2 dictionary = queue
  where featuresT1 = extractFeatures t1
        featuresT2 = extractFeatures t2
        distance = distF featuresT1 featuresT2
        queue = t2 PS.:-> distance

-- |Take the features of two 'Tweet's and return the distance as
-- 'Num'.
intersectDistance :: FeatureMap ->  FeatureMap -> Float
intersectDistance t1 t2 = totalDistanceValue
  where
    intersection = M.elems $ M.intersectionWith min t1 t2
    totalDistanceValue = foldl (+) 0 intersection


-- |Take the features of two 'Tweet's and return the distance as
-- 'Num'.
cosineDistance :: FeatureMap ->  FeatureMap -> Float
cosineDistance t1 t2 = -1000 * (mySum / (wordsInT1 * wordsInT2))
  where
    wordsInT1 = M.foldl (+) 0 t1
    wordsInT2 = M.foldl (+) 0 t2
    intersection = M.elems $ M.intersectionWith (*) t1 t2
    mySum = foldl (+) 0 intersection

-- |Takes a dictionary and a mini dictionary (frequency of words in
-- one Tweet) and calculates the idftf values for all words in the
-- mini dictionary.
idftf :: FeatureMap -> FeatureMap -> FeatureMap
idftf grandDict miniDict =  M.mapWithKey (iFrequency grandDict) miniDict

iFrequency :: FeatureMap -> String -> Float -> Float
iFrequency dict word freq = freq * (log (totalNumberOfWords / freqWord))
  where freqWord =  M.findWithDefault 1 word dict
        totalNumberOfWords = M.foldl (+) 0 dict

-- | Calculate the amount of tweets where the predicted label matches
-- the actual label.
compareLabels ::  Int -> V.Vector (Tweet,[PS.Binding Tweet Float]) ->  V.Vector Float
compareLabels k vec = V.map
                    (\(a,b) -> if (tLabel a) == getLabel k b then 1 else 0)
                    vec

compareLabelsForScheme :: [V.Vector (Tweet,[PS.Binding Tweet Float])] -> Int -> [Float]
compareLabelsForScheme vecs k = map (getAccuracy . compareLabels k) vecs

-- | Get the label for a 'Tweet' by looking at the k nearest
-- neighbors. If there are more aggressive than non_aggressive
-- 'Tweet's, the label will be aggressive, otherwise, it will be
-- non-aggressive.
getLabel :: Int -> [PS.Binding Tweet Float] -> String
getLabel k queue = if agg >= nonAgg then "aggressive" else "non_aggressive"
  where -- tweets = queueTake k queue
    tweets = take k queue
    labels = map (tLabel . PS.key) tweets
    agg = length $ filter (== "aggressive") labels
    nonAgg = length $ filter (== "non_aggressive") labels

-- | Get sum total of a vector of floats (i.e., the number of
-- correctly classified tweets) and return the accuracy
getAccuracy :: V.Vector Float -> Float
getAccuracy vec =   (V.foldl (+) 0 vec) / fromIntegral (V.length vec)

-- |TODO!
main :: IO ()
main = do
  -- Retrieve command line args (TODO: arg handler)
  (dir:_) <- getArgs

  -- Get list of files in directory dir
  files <- getFiles dir

  -- Read content of all files into list, one Text per item
  csvs <- mapM TI.readFile $ sort files

  let
    -- Add quotation marks for CSV parsing
    processedCsvs = map preprocess csvs

    -- Create Tweets from Text
    r = map parseCsv processedCsvs

    -- If parsing was successful, extract Right elements from the
    -- Either list
    tweets = rights r

    -- Create a leave-one-out cross-validation scheme
    scheme = mkCrossValScheme tweets

    -- Get all neighbors for all tweets for all schemes
    allNeighbors = map getNeighbors scheme

    ks = [1..100]

    labeledTweets = map (compareLabelsForScheme allNeighbors) ks

    results = encode labeledTweets
    -- All the work is actually done by invoking getNeigbors (nearest
    -- neighbor classifier with several ks)
    -- ks = map (getAccuracy . (compareLabels 1))

  -- Create header
    header = encode
             $ map (("fold_" ++) . show) ([1..10] :: [Integer])

  -- Write output to a file
  L.writeFile "resultsK.csv" $ header `L.append` results
