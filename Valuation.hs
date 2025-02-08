{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Valuation where

import qualified Data.Map as Map
import Data.List (sortBy)
import Rec    
import Info 

-- | Computes normalized valuations for all attributes
-- Takes raw values and returns normalized values in range 0-100
valuation :: (Ord o, Set a, Valence a) => Info o a -> Val o a
valuation info = Info $ Map.map normalize (unInfo info)
  where
    normalize rec = Rec $ Map.mapWithKey (normalizeAttr rec) (unRec rec)
    normalizeAttr rec attr val
      | valence attr = normalizePos val (getValues attr)
      | otherwise = normalizeNeg val (getValues attr)
    getValues attr = map (\r -> Map.findWithDefault 0 attr (unRec r)) 
                        (Map.elems $ unInfo info)

    normalizePos v vs = (v / sum vs) * 100
    normalizeNeg v vs = (recip v / sum (map recip vs)) * 100

-- valuation :: (Ord o, Set a, Valence a) => Info o a -> Val o a
-- valuation = onInfo (\r -> if getValence r then normalizeRec r else normalizeRecRev r)

extendBy :: (Ord o, Ord a, Valence b, Set b, Ord u,
            Expand t b u, Covers t a) =>
            Val o t -> Info a b -> Val o u
extendBy val ext = onInfo refineRec val
  where
    refineRec rec = fromList
      [(expand attr battr, v * w / 100)
      | (attr, v) <- toList rec,
        let extRec = valuation ext ! project attr,
        (battr, w) <- toList extRec]

-- | Computes total value aggregation
total :: Aggregate a b => a -> b
total = agg sum

-- | Filters records based on a specific attribute
filterByAttr :: (Ord a) => (a -> Bool) -> Rec a -> Rec a
filterByAttr p (Rec m) = Rec $ Map.filterWithKey (\k _ -> p k) m

-- | Selects records based on specific attributes
only :: (Eq a, Covers o a) => a -> Info o t -> Info o t
only a = filterInfo (\k -> project k == a)

-- | Excludes records with specific attributes
except :: (Eq a, Covers o a) => a -> Info o t -> Info o t
except a = filterInfo (\k -> project k /= a)

-- | Gets total value for each alternative
getValues :: Val o a -> Map.Map o Double
getValues = Map.map sumRec . unInfo

-- | Gets alternatives sorted by value
rankedAlternatives :: Ord o => Val o a -> [(o, Double)]
rankedAlternatives = sortByValue . Map.toList . getValues
  where 
    sortByValue = sortBy (flip (comparing snd))
    comparing f x y = compare (f x) (f y)

-- | Gets the winner (highest valued alternative)
winner :: Ord o => Val o a -> o
winner = fst . head . rankedAlternatives

-- | Gets the runner-up (second highest valued alternative)
runnerUp :: Ord o => Val o a -> o
runnerUp = fst . (!! 1) . rankedAlternatives

-- type AHPLevel =  Int 
-- -- car data is a list now.  carData = [twocarFeatures,featureOpinions,weights]
-- sensitivity carData (Honda,BMW) Personal 1 