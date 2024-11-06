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
windowItem a ng@NGram{ window = w, nCount = n } = 
  ng { window = take (n - 1) (a : w) }

incOccurence :: Ord a => a -> NGram a -> NGram a 
incOccurence event ng@NGram { nCount = n, window = w, statistics = m } 
 | length (event:w) < n = ng
 | otherwise = 
      let pattern = event : w
          occ = occurence event ng
       in ng { statistics = M.insert pattern (occ + 1) m }

chanceOf :: Ord a => a -> NGram a -> Probability
chanceOf event NGram {window = w, events = e, statistics = m} =
  let matchingPatterns = M.filterWithKey (\k _ -> take (length w) k == w) m
      sumOfMatches =     fromIntegral $ sum matchingPatterns
      occ =              fromIntegral $ M.findWithDefault 0 (event : w) m
  in (occ + 1) / (sumOfMatches + fromIntegral e)

update :: Ord a => a -> NGram a -> NGram a
update event = windowItem event . incOccurence event 

data RPP = Rock | Paper | Scissors deriving (Eq, Ord, Show)

test :: IO ()
test = do
  let ng = update Paper
         . update Rock
         . update Paper
         . update Rock
         . update Paper
         . update Rock $ ngram 3 3

  print ng
  putStrLn "Predictions"
  putStrLn $ "Rock: " ++ show (chanceOf Rock ng)
  putStrLn $ "Paper: " ++ show (chanceOf Paper ng)
  putStrLn $ "Scissors: " ++ show (chanceOf Scissors ng)
