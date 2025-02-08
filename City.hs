{-# LANGUAGE FlexibleInstances #-}
module City where

import Rec
import Info
import Valuation
import qualified Data.Map as Map
import Focus
import Explanation

-- | City alternatives 
data City = Seattle | Portland | SanFrancisco
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Living aspects to consider
data Aspect = Rent | JobMarket | Weather
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Different groups' perspectives
data Group = Student | Professional | Retiree
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Weight attribute (same as before)
data Weight = Weighted
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Required type class instances
instance Set City
instance Set Aspect
instance Set Group
instance Set Weight
instance Valence Group
instance Valence Weight

-- | Valence instance for aspects
instance Valence Aspect where
  valence Rent = False  -- Lower rent is better
  valence _ = True      -- Higher values better for jobs and weather

-- | Base city data (actual numbers)
cityAspects :: Info City Aspect
cityAspects = info [
  Seattle --> [Rent --> 2000, JobMarket --> 85, Weather --> 70],
  Portland --> [Rent --> 1600, JobMarket --> 75, Weather --> 75],
  SanFrancisco --> [Rent --> 3000, JobMarket --> 95, Weather --> 80]
  ]

-- | Different groups' priorities (scale 1-5)
aspectPriorities :: Info Aspect Group
aspectPriorities = info [
  Rent --> [Student --> 5, Professional --> 3, Retiree --> 4],
  JobMarket --> [Student --> 3, Professional --> 5, Retiree --> 1],
  Weather --> [Student --> 2, Professional --> 2, Retiree --> 5]
  ]

-- | Group weights (represents population distribution)
groupWeights :: Info Group Weight
groupWeights = info [
  Student --> weight 0.3,
  Professional --> weight 0.5,
  Retiree --> weight 0.2
  ]

-- | Helper function for weighted values
weight :: Double -> [(Weight, Double)]
weight x = [Weighted --> x]

-- | Extend valuation with groups
cityGroups :: Val City (Aspect, Group)
cityGroups = aspectVal `extendBy` aspectPriorities
  where aspectVal = mkOneTuple (valuation cityAspects)

-- | Final valuation with weights
citiesW :: Val City (Aspect, Group, Weight)
citiesW = cityGroups `extendBy` groupWeights

-- | Helper values for analysis
cities = shrinkVal citiesW :: Val City (Aspect, Group)
vd = diff cities Portland Seattle

-- | Data for sensitivity analysis
cityData :: (Info City Aspect, Info Aspect Group, Info Group Weight)
cityData = (cityAspects, aspectPriorities, groupWeights)

{-
> valuation cityAspects
{
 Seattle -> {Rent -> 34.29, JobMarket -> 33.33, Weather -> 31.11},
 Portland -> {Rent -> 42.86, JobMarket -> 29.41, Weather -> 33.33},
 SanFrancisco -> {Rent -> 22.86, JobMarket -> 37.25, Weather -> 35.56}
}

> vd = diff (valuation cityAspects) Portland Seattle

> vd
{(Rent,Student) -> 1.29, (Rent,Professional) -> 1.29, (Rent,Retiree) -> 0.69, 
 (JobMarket,Student) -> -0.35, (JobMarket,Professional) -> -0.98, (JobMarket,Retiree) -> -0.08, 
 (Weather,Student) -> 0.13, (Weather,Professional) -> 0.22, (Weather,Retiree) -> 0.22}

> total cities
{Seattle -> 33.12, Portland -> 35.54, SanFrancisco -> 31.34}

> explain cities :: Explanation City (Aspect, Group)
Portland is the best option; it is better than Seattle because
{(Rent,Professional), (Weather,Retiree)} :1.51 > |{(JobMarket,Student), (JobMarket,Professional), (JobMarket,Retiree)} :-1.41|

> explain (focus cities) :: Explanation City Aspect
Portland is the best option; it is better than Seattle because
{Rent} :3.26 > |{JobMarket} :-1.41|
-}