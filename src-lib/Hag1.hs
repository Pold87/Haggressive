{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hag
Description : Classify Tweets (aggressive vs. non_aggressive) and evaluate classification performance.
License     : None
Maintainer  : Volker Strobel (volker.strobel87@gmail.com)
Stability   : experimental
Portability : None

This module is the main interface for Tweet classification.

-}
module Hag
( module Hag -- Should be changed after debugging
 ) where

import           Control.Monad
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.Csv
import           Data.Either
import           Data.List
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Data.Text.Encoding
import qualified Data.Text.IO         as TI
import qualified Data.Vector          as V

import           NLP.Tokenize
import           Preprocess
import qualified System.Directory     as S
import           System.Environment
import           Tweethelpers

import qualified Data.PSQueue         as PS
-- import           Debug.Trace

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
getFiles dir =
  S.getDirectoryContents dir >>= return . map (dir ++) . filter (`notElem` [".", ".."])


-- |
-- = Dictionary operations
-- For convenice, I refer to two dictionaries:
-- * Mini Dictionary
-- The bag of words for /one/ Tweet
-- * Grand Dictionary
-- The bag of words for the entire Corpus
-- __TODO__: Could be defined as type.

-- |Insert an item into a 'M.Map'. Default value is 1 if the item is
-- not existing. If the item is already existing, its frequency will
-- be increased by 1.
countItem :: (Ord a) => M.Map a Float -> a -> M.Map a Float
countItem theMap item = M.insertWith (+) item 1 theMap

-- |Calculate the 'frequency' of items in a 'V.Vector' and return them
-- in a 'M.Map'.
frequency :: Ord a => V.Vector a -> M.Map a Float
frequency = V.foldl' countItem M.empty


iFrequency :: M.Map String Float -> String -> Float -> Float
iFrequency dict word freq = freq * (log (totalNumberOfWords / freqWord))
  where freqWord =  M.findWithDefault 1 word dict
        totalNumberOfWords = M.foldl (+) 0 dict

-- |Takes a mini dictionary (frequency of words in one Tweet) and a
-- dictionary and calculates the idftf values for all words in the
-- mini dictionary.
idftf :: M.Map String Float -> M.Map String Float -> M.Map String Float
idftf grandDict miniDict =  M.mapWithKey (iFrequency grandDict) miniDict

-- |Take the bag of words of two 'Tweet's and return the distance as
-- 'Num'. /TODO/: Forgot it.
intersectDistance :: (Num a) => M.Map String a -> M.Map String a -> a
intersectDistance t1 t2 = foldl (+) 0 $ M.elems $ M.intersectionWith (+) t1 t2

-- |Extract features (for the bag of words) for one Tweet.
-- Thereby, the Tweet will be (in order of application):
-- * tokenized
-- * converted to a 'V.Vector'
-- * 'String's will be converted to lowercase
-- * 'String's that are not 'isAlpha' are removed
-- * 'String's that are element of 'stopWords' are removed
-- * Empty 'String's will be removed
tweetToMiniDict :: Tweet -> M.Map String Float
tweetToMiniDict  = frequency
                   . V.filter (/= "")
                   . V.filter (`notElem` stopWords)
                   . V.map (filter isAlpha . map toLower)
                   . V.fromList
                   . tokenize
                   . tMessage

-- |Take a 'Grand Dictionary'
insertInMap :: M.Map Tweet (M.Map String Float) -> Tweet -> M.Map Tweet (M.Map String Float)
insertInMap oldMap tweet = M.insert tweet val oldMap
  where val = tweetToMiniDict tweet

-- |Specify k (the number of neighbors) and compare two vectors of
-- 'Tweet's and return the k nearest neighbors for each 'Tweet'.
validate :: Int -- value of k
            -> (V.Vector Tweet, V.Vector Tweet) -- vector of (test, train)
            -> V.Vector (Tweet,[Tweet]) -- vector
validate k (v1,v2) =  V.map (crossCheckBetterK k miniDictV2) v1
  where miniDictV2 = V.foldl insertInMap M.empty v2 :: M.Map Tweet (M.Map String Float)

-- |TODO!
crossCheckBetterK :: Int
                     -> M.Map Tweet (M.Map String Float)
                     -> Tweet
                     -> (Tweet, [Tweet])
crossCheckBetterK k tweetMap tweet = mini
  where mini = featureIntersectionBetterK k tweetMap tweet

-- |TODO!
featureIntersectionBetterK :: Int
                              -> M.Map Tweet (M.Map String Float)
                              -> Tweet
                              ->  (Tweet, [Tweet])
featureIntersectionBetterK k tweetMap tweet = (tweet, mini)
  where mini = map PS.key
               $ take k
               $ PS.toAscList
               $ PS.fromList
               $ M.elems
               $ M.mapWithKey (mergeTweetFeatures intersectDistance tweet) tweetMap

-- | Take the first k elements of a queue
queueTake :: Int -> PS.PSQ Tweet Float -> [Tweet]
queueTake k queue = queueTake' k queue []

-- |TODO!
queueTake' :: Int -> PS.PSQ Tweet Float -> [Tweet] -> [Tweet]
queueTake' 0 _ acc = acc
queueTake' k queue acc = case mini of
  Nothing -> error "Empty queue"
  Just m -> queueTake' (k - 1) (PS.deleteMin queue) (PS.key m:acc)
  where mini = PS.findMin queue

-- |TODO!
mergeTweetFeatures :: (M.Map String Float -> M.Map String Float -> Float)
                      -> Tweet
                      -> Tweet
                      -> M.Map String Float
                      -> PS.Binding Tweet Float
mergeTweetFeatures distF t1 t2 mdt2 = t2 PS.:-> distance
  where mdt1 = idftf mdt2 $ tweetToMiniDict t1  -- Feature extraction Tweet 1
        distance = negate $ distF mdt1 mdt2

-- TODO: CHECK!! Seems to work sometimes with a == b, i.e. there are
-- identical tweets?!

-- | Calculaute the amount of tweets where the predicted label matches
-- the actual label.
crossCheckRealK ::  V.Vector (Tweet,[Tweet]) -> V.Vector Float
crossCheckRealK vec = V.map (\(a,b) -> if (tLabel a) == getCategoryK b then 1 else 0) vec

-- |TODO!
getCategoryK :: [Tweet] -> String
getCategoryK tweets = if agg > nonAgg then "aggressive" else "non_aggressive"
  where categories = map tLabel tweets
        agg = length $ filter (== "aggressive") categories
        nonAgg = length $ filter (== "non_aggressive") categories

-- |TODO!
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

    -- All the work is actually done by invoking validate (k-nearest
    -- neighbor classifier with several ks)
    k1 = map (getAccuracy . crossCheckRealK . validate 1) scheme

  -- Print result
  print k1


-- queueTake k
