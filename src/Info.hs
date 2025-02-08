{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Info where

import Text.Printf
import qualified Data.Map as Map
import Data.List 
import Rec 

-- | Core info type mapping objects to their attribute records
data Info o a = Info {unInfo :: Map.Map o (Rec a)}
  deriving (Eq, Ord)

mkInfo :: Map.Map o (Rec a) -> Info o a 
mkInfo = Info

-- | Type for normalized valuations
type Val o a = Info o a


-- Show instance for Info (object-to-records)
instance (Show o, Show a, Ord a, Ord o) => Show (Info o a) where
  show (Info m) =
    if Map.null m
      then "{}"
      else "{\n" ++ entries ++ "\n}"
    where
      entries = intercalate ",\n" formatted
      formatted = map showEntry (Map.toAscList m)
      showEntry (k, v) = " " ++ show k ++ " -> " ++ show v

onInfo :: (Rec a -> Rec b) -> Info o a -> Info o b
onInfo f =  mkInfo. Map.map f. unInfo

-- | Generic operations on Info
mapInfo :: Ord b => (a -> b) -> Info o a -> Info o b
mapInfo f = onInfo (mapRec f)

mapInfoVal :: (Double -> Double) -> Info o a -> Info o a  
mapInfoVal f = onInfo (mapRecVal f)

filterInfo :: (o -> Bool) -> Info o a -> Info o a
filterInfo p = mkInfo . Map.filterWithKey (\k _ -> p k) . unInfo


foldInfo :: (Rec a -> Rec a -> Rec a) -> Rec a -> Info o a -> Rec a
foldInfo f init (Info m) = Map.foldr f init m

zipInfo :: Ord o => (Rec a -> Rec b -> Rec c) -> Info o a -> Info o b -> Info o c
zipInfo f (Info m1) (Info m2) = Info $ Map.intersectionWith f m1 m2

unionInfo :: Ord o => Info o a -> Info o a -> Info o a
unionInfo (Info m1) (Info m2) = Info $ Map.union m1 m2

projLeft :: (Ord o, Ord a, Ord b) => Val o (a,b) -> Val o a
projLeft (Info m) = Info $ Map.map (\(Rec r) -> Rec $ Map.mapKeys fst r) m

projRight :: (Ord o, Ord a, Ord b) => Val o (a,b) -> Val o b
projRight (Info m) = Info $ Map.map (\(Rec r) -> Rec $ Map.mapKeys snd r) m

lookupInfoTup :: (Ord o, Ord a, Ord b) => (o,(a,b)) -> Val o (a,b) -> Double
lookupInfoTup (o,ab) val = maybe 0 id $ Map.lookup ab . unRec $ val ! o

lookupInfo :: (Ord o, Ord a) => (o,a) -> Val o a -> Double
lookupInfo (o,a) val = maybe 0 id $ Map.lookup a . unRec $ val ! o

infoToList :: Info o a -> [(o, [(a, Double)])]
infoToList (Info m) = [(k, toList v) | (k,v) <- Map.toList m]

infoFromList :: (Ord o, Ord a) => [(o, [(a, Double)])] -> Info o a
infoFromList = Info . Map.fromList . map (fmap fromList)

-- | Helper functions for value construction
info :: (Ord o, Ord a) => [(o, [(a, Double)])] -> Info o a 
info = Info . Map.fromList . map (fmap (Rec . Map.fromList))

(-->) :: a -> b -> (a, b)
x --> y = (x, y)

-- | Record lookup in Info mapping (core operation)
(!) :: (Eq o,Ord o) => Info o a -> o -> Rec a
m ! k = case Map.lookup k (unInfo m) of
  Just v -> v
  Nothing -> error "Key not found"

-- | Computes differences between two records for all attributes
diff :: (Ord o, Ord r) => Info o r -> o -> o -> Rec r
diff m o1 o2 = (m ! o1) - (m ! o2)

class Aggregate a b | a -> b where
  agg :: ([Double] -> Double) -> a -> b

-- | Core instances for value aggregation
instance (Ord a) => Aggregate (Rec a) (Agg a) where
  agg f r = Agg (Map.keys $ unRec r) (f $ Map.elems $ unRec r)

instance (Ord o, Ord a) => Aggregate (Val o a) (Rec o) where
  agg f v = Rec $ Map.map (f . Map.elems . unRec) (unInfo v)

-- | Represents a single attribute 
data OneTuple a = OneTuple a deriving (Show, Eq, Ord)

-- | Helper for single tuple construction
mkOneTuple :: Ord a => Val o a -> Val o (OneTuple a)
mkOneTuple = onInfo (mapRec OneTuple)

-- | For working with tuples in multi-dimensional attributes
class Covers t a | t -> a where
  project :: t -> a

class Expand t a u | t a -> u where
  expand :: t -> a -> u

-- | Basic tuple instances
instance Covers (a,b) a where project = fst
instance Covers (a,b) b where project = snd
instance Covers (OneTuple a) a where project (OneTuple x) = x 
instance Covers (a,b,c) a where project (a,_,_) = a
instance Covers (a,b,c) b where project (_,b,_) = b
instance Covers (a,b,c) c where project (_,_,c) = c

instance Covers (a,b,c,d) a where project (a,_,_,_) = a
instance Covers (a,b,c,d) b where project (_,b,_,_) = b
instance Covers (a,b,c,d) c where project (_,_,c,_) = c
instance Covers (a,b,c,d) d where project (_,_,_,d) = d

instance Expand (OneTuple a) b (a,b) where expand (OneTuple a) b = (a,b)
instance Expand (a,b) c (a,b,c) where expand (a,b) c = (a,b,c)

-- | Num instance for records to enable arithmetic operations
instance (Ord a) => Num (Rec a) where
  (+) (Rec m1) (Rec m2) = Rec $ Map.unionWith (+) m1 m2
  (-) (Rec m1) (Rec m2) = Rec $ Map.unionWith (-) m1 m2 
  (*) (Rec m1) (Rec m2) = Rec $ Map.intersectionWith (*) m1 m2
  abs (Rec m) = Rec $ Map.map abs m
  signum (Rec m) = Rec $ Map.map signum m
  fromInteger n = Rec Map.empty


-- | Class for shrinking tuple types
class Shrink t t' | t -> t' where 
  shrink :: t -> t'

instance Shrink (OneTuple a) () where 
  shrink _ = ()

instance Shrink (a,b) b where 
  shrink = snd

instance Shrink (a,b) a where 
  shrink = fst

instance Shrink (a,b,c) (b,c) where 
  shrink (a,b,c) = (b,c)

instance Shrink (a,b,c) (a,c) where 
  shrink (a,b,c) = (a,c)

instance Shrink (a,b,c) (a,b) where 
  shrink (a,b,c) = (a,b)

instance Shrink (a,b,c,d) (b,c,d) where 
  shrink (a,b,c,d) = (b,c,d)

instance Shrink (a,b,c,d) (a,c,d) where 
  shrink (a,b,c,d) = (a,c,d)

instance Shrink (a,b,c,d) (a,b,d) where 
  shrink (a,b,c,d) = (a,b,d)

instance Shrink (a,b,c,d) (a,b,c) where 
  shrink (a,b,c,d) = (a,b,c)

-- | Class for splitting tuple types
class (Covers t a, Shrink t t') => Split t a t' where {}

instance Split (OneTuple a) a ()
instance Split (a,b) a b
instance Split (a,b) b a
instance Split (a,b,c) a (b,c)
instance Split (a,b,c) b (a,c)
instance Split (a,b,c) c (a,b)
instance Split (a,b,c,d) a (b,c,d)
instance Split (a,b,c,d) b (a,c,d)
instance Split (a,b,c,d) c (a,b,d)
instance Split (a,b,c,d) d (a,b,c)

-- | Shrinks valuations by removing one dimension
shrinkVal :: (Ord o, Ord a, Ord a', Shrink a a') => Val o a -> Val o a'
shrinkVal (Info m) = Info $ Map.map shrinkRec m
  where
    shrinkRec (Rec r) = Rec $ Map.mapKeys shrink r