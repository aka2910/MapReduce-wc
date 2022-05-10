module MapReduce where

import Control.Parallel (pseq)
import Control.Parallel.Strategies ( parList, using, Strategy )

-- map reduce module

seqMapReduce :: (a -> b) -> ([b] -> c) -> [a] -> c
seqMapReduce mAP rEDUCE = rEDUCE . map mAP

parMapReduce ::
  Strategy b -> -- for mapping
  (a -> b) -> -- mAP func
  Strategy c -> -- for reducing
  (b -> c) -> -- rEDUCE func
  [a] -> -- init list
  [c]
parMapReduce mAPstrat mAP rEDstrat rEDUCE xs =
  pseq mres rres
  where
    mres = map mAP xs `using` parList mAPstrat
    rres = map rEDUCE mres `using` parList rEDstrat -- [[(B.ByteString, Int)]]

-- parList: evaluates each element  of a list in parallel
-- pseq: like seq, but parallel and more control over order of evaluation
