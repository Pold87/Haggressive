module Tweets
  (filterByLabel
, Tweet(..)) where

import           Control.Applicative ((<$>), (<*>), (<|>))
import           Control.Monad       (mzero)
import           Data.Csv
import qualified Data.PSQueue        as PS
import qualified Data.Vector         as V

-- | Parsing Record to Tweet
instance FromRecord Tweet where
  parseRecord v
         | V.length v == 5 = Tweet <$>
                             v.! 0 <*>
                             v.! 1 <*>
                             v.! 2 <*>
                             v.! 3 <*>
                             v.! 4
         | otherwise = mzero

-- | A Tweet consists of a category, a user, a date, a time, and a
-- message
data Tweet = Tweet { tLabel   :: String
                   , tUser    :: String
                   , tDate    :: String
                   , tTime    :: String
                   , tMessage :: String
                   } deriving (Show, Eq, Ord)


-- | Filter 'Tweet's by label
filterByLabel :: V.Vector Tweet -> String -> V.Vector Tweet
filterByLabel tweets label = V.filter (\t -> tLabel t == label) tweets


