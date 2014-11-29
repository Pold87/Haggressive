module Evaluation
       (Evaluation.main)
       where

import           Control.Monad
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Char
import           Data.Csv
import           Data.Either
import           Data.List
import qualified Data.Map                   as M
import           Data.Ord
import qualified Data.Text                  as T
import           Data.Text.Encoding
import qualified Data.Text.IO               as TI
import qualified Data.Vector                as V
import           Hag
import           NLP.Tokenize
import           Preprocess
import           Preprocess
import qualified System.Directory           as S
import           System.Environment
import           Tweethelpers
import           Tweethelpers

import qualified Data.PSQueue               as PS
-- import           Debug.Trace

-- |Create a dictionary ('M.Map String Float' /TODO/: probably define
-- dictionary type) from a 'V.Vector' of 'Tweet's
createDictionary :: V.Vector Tweet -> M.Map String Float
createDictionary tweets = V.foldl (M.unionWith (+)) M.empty allTweets
   where allTweets = V.map tweetToMiniDict tweets

-- |Create a 'grand dictionary' ('M.Map String Float' /TODO/: probably
-- define dictionary type) from a 'mini dictionary' ('M.Map Tweet
-- (M.Map String Float)'
createDictionaryFromMap :: M.Map Tweet (M.Map String Float) -> M.Map String Float
createDictionaryFromMap tweetMap = foldl (M.unionWith (+)) M.empty $ M.elems tweetMap

main :: IO ()
main = do
  (dir:_) <- getArgs

  files <- getFiles dir
  csvs <- mapM TI.readFile $ sort files
  let processedCsvs = map preprocess csvs
      r = map parseCsv processedCsvs
      listOfVectorOfTweets = rights r  :: [V.Vector Tweet]
      -- Tweet vectors
      vTweets = V.concat listOfVectorOfTweets :: V.Vector Tweet
      aggTweets = filterByLabel vTweets "aggressive" :: V.Vector Tweet
      nonAggTweets = filterByLabel vTweets "non_aggressive" :: V.Vector Tweet

      -- Tweet dictionaries
      dict = createDictionary vTweets
      aggDict = createDictionary aggTweets
      nonAggDict = createDictionary nonAggTweets

      words = (encode $ sortBy (comparing $ snd) $
              M.toList dict)  :: L.ByteString

      aggWords = (encode $ sortBy (comparing $ snd) $
              M.toList aggDict)  :: L.ByteString

      nonAggWords = (encode $ sortBy (comparing $ snd) $
              M.toList nonAggDict)  :: L.ByteString


      header = BSL.pack "word,frequency\r\n" :: BSL.ByteString

  -- Write output to csv
  L.writeFile "agg_words.csv" $ header `L.append` aggWords
  L.writeFile "non_agg_words.csv" $ header `L.append` nonAggWords
  L.writeFile "words.csv" $ header `L.append` words
  print $ V.length aggTweets


