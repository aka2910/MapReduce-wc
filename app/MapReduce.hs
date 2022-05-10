module MapReduce where

import Control.Parallel(pseq)
import Control.Parallel.Strategies
    ( using, Strategy, evalBuffer )


-- map reduce library

seqMapReduce :: (a   -> b) -> ([b] -> c) -> [a] -> c
seqMapReduce mAP rEDUCE = rEDUCE . map mAP

parMapReduce
    :: Strategy b  -- for mapping
    -> (a   -> b)  -- mAP func
    -> Strategy c  -- for reducing
    -> (b -> c)  -- rEDUCE func
    -> [a]         -- init list
    -> [c]
parMapReduce mAPstrat mAP rEDstrat rEDUCE xs =
    pseq mres rres
  where mres = map mAP xs `using` evalBuffer 20 mAPstrat
        rres = map rEDUCE mres `using` evalBuffer 20 rEDstrat  -- [[(B.ByteString, Int)]]

-- pseq: like seq, but parallel and more control over order of evaluation
