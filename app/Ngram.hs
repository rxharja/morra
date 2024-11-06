module Ngram where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Events = Int

type N = Int

type Probability = Double

type ProbabilityThreshold = Double

type Occurence = Int

type RecordedEvents a = M.Map [a] Occurence

data NGram a =
  NGram { events :: Events
        , nCount :: N
        , window :: [a]
        , statistics :: RecordedEvents a }
  deriving Show

ngram :: Events -> N -> NGram a
ngram e n = NGram { events = e, nCount = n, window = [], statistics = M.empty }

occurence :: Ord a => a -> NGram a -> Occurence
occurence event (NGram {window = w, statistics = m }) = 
  fromMaybe 0 (M.lookup (event:w) m)

windowItem :: a -> NGram a -> NGram a
windowItem a ng@NGram{ window = w, nCount = n } = ng { window = a : shiftedWindow }
  where
    windowLen = length w
    shiftedWindow = if windowLen >= (n - 1)
                    then take (windowLen - 1) w
                    else w

incOccurence :: Ord a => a -> NGram a -> NGram a 
incOccurence event ng@NGram { window = w, nCount = n, statistics = m } = 
  let t = length w - (n - 1)
      pattern = event : w
      occ = occurence event ng
   in if t < 0 
      then ng 
      else ng { statistics = M.insert pattern (occ + 1) m }

update :: Ord a => a -> NGram a -> NGram a 
update event = incOccurence event . windowItem event

eventChance :: Ord a => [a] -> NGram a -> Probability
eventChance event (NGram {events = e, nCount = n, statistics = m}) =
  let occ = fromMaybe 0 (M.lookup event m)
   in fromIntegral occ / (fromIntegral n^e)
