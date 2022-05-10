module Main where

import System.IO ( stdout, hFlush )
import Wc ( sort, wcseq, wcpar )
import qualified Data.ByteString.Lazy.Char8 as B
import System.TimeIt ( timeIt )


main :: IO()
main = do
    putStr "Enter a file name: "
    hFlush stdout
    fileName <- getLine
    content <- B.readFile fileName
    -- print $ take 10 $ sort $ wcpar content
    putStr "Sequential: "
    timeIt $ print $ take 10 $ sort $ wcseq content
    timeIt $ print $ take 10 $ sort $ wcpar content
    -- timeIt $ print $ take 10 $ sort $ pipeline 10000 content
