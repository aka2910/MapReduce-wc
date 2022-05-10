module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import System.IO (hFlush, stdout)
import System.TimeIt (timeIt)
import Wc (sort, wcpar, wcseq)

main :: IO ()
main = do
  putStr "Enter a file name: "
  hFlush stdout
  fileName <- getLine
  content <- B.readFile fileName
  -- print $ take 10 $ sort $ wcpar content
  putStr "Sequential: "
  timeIt $ print $ take 10 $ sort $ wcseq content
  putStr "Parallel: "
  timeIt $ print $ take 10 $ sort $ wcpar content

-- timeIt $ print $ take 10 $ sort $ pipeline 10000 content
