{-# LANGUAGE FlexibleContexts #-}
module Focus where

import qualified Data.Map as Map
import Rec
import Info 
import Valuation

class Focusing f where
  focus :: (Ord t, Ord t', Shrink t t') => f t -> f t'

instance Focusing Rec where
  focus (Rec m) = Rec (Map.fromListWith (+)
                          [ (shrink k, d)
                          | (k,d) <- Map.toList m ])

instance Focusing (Info o) where
  focus (Info m) = Info (Map.map focus m)

dimensionContribution :: (Ord o, Ord a) => Val o a -> [(a, Double)]
dimensionContribution  = toList. foldInfo (+) (Rec Map.empty) 

-- | Value difference impact calculation
impactAnalysis :: Ord a => Rec a -> [(a, String)]
impactAnalysis vd = 
  [(attr, showPercentage $ (val / total) * 100) 
  | (attr, val) <- Map.toList (unRec vd)]
  where
    total = sumRec $ mapRecVal abs vd 
    showPercentage x = show (round x :: Integer) ++ "%"

compareCategories :: (Ord o, Ord a) => Val o a -> o -> o -> [(a, Ordering)]
compareCategories val o1 o2 = 
  [(attr, compare (lookupInfo (o1,attr) val) (lookupInfo (o2,attr) val)) 
   | attr <- Map.keys . unRec $ val ! o1]