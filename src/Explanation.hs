{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Explanation where

import qualified Data.Map as Map
import Rec 
import Info 
import Valuation
import Data.List 
import Focus 

data Dominance a = Dominance (Agg a) (Agg a)

instance Show a => Show (Dominance a) where
  show (Dominance pos neg) = 
    show pos ++ " > |" ++ show neg ++ "|"

data Explanation o a = Explanation o o (Dominance a)

instance (Show o, Show a) => Show (Explanation o a) where
  show (Explanation winner runnerup dom) =
    show winner ++ " is the best option; it is better than " ++ 
    show runnerup ++ " because\n" ++ show dom

dominators :: Ord a  => Rec a -> [Rec a]
dominators vd = filter isDominator allPossibleSets
  where
    totalDiff = sumRec vd
    barrierTotal = sumRec (barrier vd)
    isDominator set = sumRec set > abs barrierTotal
    allPossibleSets = powersetRec $ filterRecVal (> 0) vd

mds :: Ord a => Rec a -> [Rec a]
mds vd = filter isMinimal (dominators vd)
  where
    isMinimal d = not $ any (\d' -> isSubset d' d && d' /= d) (dominators vd)
    isSubset r1 r2 = all (\(k,v) -> Map.lookup k (unRec r2) == Just v) (toList r1)

barrier :: Ord a => Rec a -> Rec a
barrier = filterRecVal (< 0)

explain :: (Ord o, Ord a) => Val o a -> Explanation o a
explain val = Explanation w r dom
  where
    w = winner val
    r = runnerUp val
    dom = dominance $  diff val w r

dominance :: Ord a => Rec a -> Dominance a
dominance vd = Dominance (agg sum md) (agg sum $ barrier vd)
  where md = head $ mds vd 

factor :: (Ord t, Ord a, Ord t', Split t a t') => Rec t -> Factor a t'
factor rec = Factor $ Map.fromListWith combine
  [(k, (v, makeInnerRec k' v rec)) 
   | (k', v) <- toList rec,
     let k = project k']
  where
    combine (v1,r1) (v2,r2) = (v1 + v2, unionRec (+) r1 r2)
    
    makeInnerRec k' val orig = 
      let grouped = groupByProjection orig
          total = Map.findWithDefault 0 (project k') grouped
          innerVal = Map.singleton (shrink k') (val * 100 / total)
      in Rec innerVal
    
    groupByProjection orig = 
      Map.fromListWith (+)
      [(project k, v) | (k,v) <- toList orig]

impact :: Ord a => Rec a -> Rec a 
impact vd = fromList $ map calcImpact (Map.keys $ unRec vd)
  where
    total = sum $ map abs $ Map.elems $ unRec vd
    calcImpact a = (a, maybe 0 (\v -> abs v / total * 100) (Map.lookup a $ unRec vd))

powersetRec :: Ord a => Rec a -> [Rec a]
powersetRec = map fromList . powerset . toList

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = ps ++ map (x:) ps where ps = powerset xs

data Factor k r = Factor { unFactor :: Map.Map k (Double, Rec r) }


instance (Show a, Show t, Ord t) => Show (Factor a t) where
  show (Factor m) = 
    if Map.null m 
    then "{}"
    else "{" ++ intercalate ",\n " entries ++ "}"
    where
      total = sum $ map (fst . snd) $ Map.toList m
      entries = map showEntry (Map.toAscList m)
      showEntry (k, (v, rec)) = 
        show k ++ " -> " ++ 
        show (round ((v / total) * 100)) ++ "% " ++ 
        showInnerRec rec
      showInnerRec (Rec r) = 
        "{" ++ intercalate ", " 
        [show k ++ " -> " ++ show (round v) ++ "%" 
         | (k,v) <- Map.toList r] ++ "}"

         
factorBy :: (Ord k, Ord r) => (t -> k) -> (t -> r) -> Rec t -> Factor k r 
factorBy proj rem (Rec m) = Factor normalized
  where
    groups = Map.foldrWithKey combine Map.empty m
      where combine t v  = Map.insertWith (++) (proj t) [(rem t, v)] 
              
    normalized = Map.map normalizeGroup groups
      where
        normalizeGroup lst =
          let groupSum = sum (map snd lst)
              normList = [(r, (v / groupSum) * 100) | (r, v) <- lst]
          in (100, fromList normList)