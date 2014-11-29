-- From Hag.hs
---------------

-- TODO: CHECK!! Seems to work sometimes with a == b, i.e. there are identical tweets?!
crossCheckReal ::  V.Vector (Tweet,Tweet,Float) -> V.Vector Float
crossCheckReal vec = V.map (\(a,b,_) -> if (tLabel a) == (tLabel b) then 1 else 0) vec

endList ::  [Float] -> Float
endList xs =   (foldl (+) 0 xs) / fromIntegral (length xs)


-- From Tweethelpers.hs
-----------------------
