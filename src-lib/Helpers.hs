module Helpers
       (mkCrossValScheme
       , queueTake
       , stopWords)
       where

import qualified Data.PSQueue     as PS
import qualified Data.Vector      as V
import           Debug.Trace
import           System.IO.Unsafe

-- | 'FilePath' of the 'stopWords' that are removed before the
-- dictionary is created
stopWordsFile :: FilePath
stopWordsFile = "../dutch-stop-words.txt"

-- | Read the 'stopWords' from the 'stopWordsFile'
stopWords :: [String]
stopWords =  lines file
  where file = unsafePerformIO $ readFile stopWordsFile


-- | Make a cross-validation scheme from a list of vectors
mkCrossValScheme :: (Eq a) =>  [V.Vector a] -> [(V.Vector a,V.Vector a)]
mkCrossValScheme xs = map (leaveOneOut xs) xs

-- | Create pair of a list of vectors and a vector that specifies
-- which vector should be left out
leaveOneOut :: (Eq a) => [V.Vector a] -> V.Vector a -> (V.Vector a,V.Vector a)
leaveOneOut all test = (test, V.concat $ filter (/= test) all)

-- | Count the number of occurrences of an item in a list
--countItem elem = length . filter (==elem)

-- | Get total amount of exclamation marks in a list



-- | Get total amount of capitalized characters in a list

-- | Take the first k elements of a queue
queueTake :: (Ord k, Ord p, Show k) => Int -> PS.PSQ k p -> [k]
queueTake k queue = queueTake' k queue []

-- | Helper function for 'queueTake'
queueTake' :: (Ord k, Ord p) => Int -> PS.PSQ k p -> [k] -> [k]
queueTake' 0 _ acc = acc
queueTake' k queue acc = case mini of
  Nothing -> []
  Just m -> queueTake' (k - 1) (PS.deleteMin queue) (PS.key m:acc)
  where mini = PS.findMin queue
