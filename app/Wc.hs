{-# LANGUAGE TupleSections #-}

module Wc where

import Control.Parallel.Strategies ( rseq )
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isAlpha, toLower)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map
  ( Map,
    fromListWith,
    toList,
    unionsWith,
  )
import MapReduce (parMapReduce, seqMapReduce)

-- seq
wcseq :: B.ByteString -> [(B.ByteString, Int)]
wcseq = seqMapReduce wcmAP wcrEDUCE . splitAndProcess 100000

-- frequency of each letter in the Byte String
-- sequential :: B.ByteString -> [(B.ByteString, Int)]
-- sequential bs =  toList $ fromListWith (+) [(c, 1) | c <- map removeNonAlphabet $ B.words bs]

-- word count functions
wcmAP :: [B.ByteString] -> [(B.ByteString, Int)]
wcmAP = map (,1)

wcrEDUCE :: [[(B.ByteString, Int)]] -> [(B.ByteString, Int)]
wcrEDUCE = toList . fromListWith (+) . concat

-- parallel
wcpar :: B.ByteString -> [(B.ByteString, Int)]
wcpar = finalreduce . parMapReduce rseq wcmAP rseq parwcrEDUCE . splitAndProcess 100000

-- rdeepseq: A Strategy that fully evaluates its argument
-- rseq: A srategy that evaluates its argument to whnf
-- whnf : expression evaluated to outermost data constructor or lambda abstraction.

parwcrEDUCE :: [(B.ByteString, Int)] -> Map B.ByteString Int
parwcrEDUCE = fromListWith (+)

finalreduce :: [Map B.ByteString Int] -> [(B.ByteString, Int)]
finalreduce = toList . unionsWith (+)

-- Helper functions

sort :: Ord b => [(a, b)] -> [(a, b)]
sort = sortBy (flip compare `on` snd)

-- `on` it transforms two inputs and then combines the outputs.

splitAndProcess :: Int -> B.ByteString -> [[B.ByteString]]
splitAndProcess n bs = chunk n $ map removeNonAlphabet $ B.words bs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n lst = as : chunk n bs
  where
    (as, bs) = splitAt n lst

removeNonAlphabet :: B.ByteString -> B.ByteString
removeNonAlphabet = B.filter isAlpha . B.map toLower
