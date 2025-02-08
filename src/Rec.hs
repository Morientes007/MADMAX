{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Rec where

import Text.Printf
import qualified Data.Map as Map
import Data.List 

-- | Core record type mapping attributes to values
data Rec a = Rec {unRec :: Map.Map a Double}
  deriving (Eq, Ord)

mkRec :: Map.Map a Double -> Rec a  
mkRec = Rec 

-- | Generic operations on Rec
mapRec :: (Ord b) => (a -> b) -> Rec a -> Rec b
mapRec f (Rec m) = Rec $ Map.mapKeys f m

-- | Generic operations on Rec
mapRecVal ::  (Double -> Double) -> Rec a -> Rec a 
mapRecVal f  = mkRec . Map.map f . unRec 

normalizeRec :: Ord a => Rec a -> Rec a
normalizeRec rec = mapRecVal (\x -> x * 100 / sumRec rec) rec

normalizeRecRev :: Ord a => Rec a -> Rec a
normalizeRecRev rec = mapRecVal (\x -> recip x * 100 / sumRec (mapRecVal recip rec)) rec

getValence :: (Valence a, Ord a) => Rec a -> Bool
getValence (Rec m) = valence (head $ Map.keys m)

lookupRec :: Ord o => o -> Rec o -> Double
lookupRec k (Rec m) = Map.findWithDefault 0 k m

-- In BaseOps or in your Types module, add:
filterRecVal :: (Double -> Bool) -> Rec a -> Rec a
filterRecVal p (Rec m) = Rec $ Map.filter p m

foldRec :: (Double -> Double -> Double) -> Double -> Rec a -> Double
foldRec f init (Rec m) = Map.foldr f init m

sumRec :: Rec a -> Double
sumRec = foldRec (+) 0

avgRec :: Rec a -> Double
avgRec r@(Rec m) = sumRec r / fromIntegral (Map.size m)

zipRec :: Ord a => (Double -> Double -> Double) -> Rec a -> Rec a -> Rec a
zipRec f (Rec m1) (Rec m2) = Rec $ Map.intersectionWith f m1 m2

unionRec :: Ord a => (Double -> Double -> Double) -> Rec a -> Rec a -> Rec a
unionRec f (Rec m1) (Rec m2) = Rec $ Map.unionWith f m1 m2

instance (Show a, Ord a) => Show (Rec a) where
  show (Rec m) = 
    if Map.null m 
      then "{}"
      else "{" ++ intercalate ", " entries ++ "}"
    where
      entries = map showEntry (Map.toAscList m)
      showEntry (k, v) = show k ++ " -> " ++ (printf "%.2f" v :: String)

-- | Generic transformations between types
toList :: Rec a -> [(a, Double)]
toList (Rec m) = Map.toList m

fromList :: Ord a => [(a, Double)] -> Rec a
fromList = Rec . Map.fromList


-- | For aggregating values with their attributes
data Agg a = Agg [a] Double
  deriving Eq

instance (Show a) => Show (Agg a) where 
  show (Agg ls v) = "{" ++ intercalate ", " (map show ls) ++ "} :" ++ 
                     (printf "%.2f" v :: String)

-- | Core typeclasses for value handling
class Ord a => Valence a where
  valence :: a -> Bool
  valence _ = True

class (Bounded a, Enum a, Ord a) => Set a where
  members :: [a]
  members = enumFromTo minBound maxBound

