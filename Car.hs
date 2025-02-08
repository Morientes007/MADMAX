{-# LANGUAGE FlexibleInstances #-}
module Car where

import Rec 
import Info 
-- import BaseOps
import Valuation
import qualified Data.Map as Map
import Focus 
import Explanation 


-- | Car alternatives
data Car = Honda | BMW | Toyota
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Feature attributes
data Feature = Price | MPG | Safety
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Opinion attributes
data Opinion = Personal | Expert
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Weight attribute
data Weight = Weighted
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Type class instances
instance Set Car
instance Set Feature
instance Set Opinion
instance Set Weight
instance Valence Opinion
instance Valence Weight

-- | Valence instance for features
instance Valence Feature where
  valence Price = False  -- Lower price is better
  valence _     = True   -- Higher values are better for MPG and Safety

-- | Basic car features
carFeatures :: Info Car Feature
carFeatures = info [
  Honda  --> [Price --> 34000, MPG --> 30, Safety --> 9.8],
  BMW    --> [Price --> 36000, MPG --> 32, Safety --> 9.1]]


featureDiff = diff carFeatures Honda BMW

-- | Basic car features
threeCars :: Info Car Feature
threeCars = info [
  Honda  --> [Price --> 34000, MPG --> 30, Safety --> 9.8],
  BMW    --> [Price --> 36000, MPG --> 32, Safety --> 9.1],
  Toyota --> [Price --> 27000, MPG --> 30, Safety --> 9.4]]

-- | Feature opinions (weights)
featureOpinions :: Info Feature Opinion
featureOpinions = info [
  Price  --> [Personal --> 5, Expert --> 3],
  MPG    --> [Personal --> 3, Expert --> 5],
  Safety --> [Personal --> 2, Expert --> 2]]

-- | Opinion weights
weights :: Info Opinion Weight
weights = info [
  Personal --> [Weighted --> 0.6],
  Expert   --> [Weighted --> 0.4]]

-- | Helper function for creating weighted values
weight :: Double -> [(Weight, Double)]
weight x = [Weighted --> x]

-- | Extend valuation with opinions
carOpinions :: Val Car (Feature, Opinion)
carOpinions = featureVal `extendBy` featureOpinions

-- | Final valuation with weights
carsW :: Val Car (Feature, Opinion, Weight)
carsW = carOpinions `extendBy` weights

valsThreeCars = valuation threeCars
vd3 = diff (valuation threeCars) Honda BMW

cars = shrinkVal carsW :: Val Car (Feature,Opinion)
vd = diff (valuation carFeatures) Honda BMW

exps :: Explanation Car (Feature,Opinion)
exps = explain cars 

featureVal :: Val Car (OneTuple Feature)
featureVal = mkOneTuple (valuation carFeatures)

-- carData ::  Val3 Car Feature Opinion Weight
carData = (carFeatures,featureOpinions,weights)

myvd = diff (valuation carFeatures) Honda BMW

honda:_ = mds vd'
bmw  = barrier vd' 
vd' = diff cars Honda BMW

