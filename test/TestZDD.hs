{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module TestZDD (zddTestGroup) where

import Control.Monad
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Data.DecisionDiagram.BDD.Internal as Internal
import Data.DecisionDiagram.ZDD (ZDD (..))
import qualified Data.DecisionDiagram.ZDD as ZDD

-- ------------------------------------------------------------------------

instance ZDD.ItemOrder a => Arbitrary (ZDD a) where
  arbitrary = liftM ZDD $ do
    vars <- liftM (sortBy (ZDD.compareItem (Proxy :: Proxy a)) . IntSet.toList . IntSet.fromList) arbitrary
    let f vs n = oneof $
          [ return Internal.F
          , return Internal.T
          ]
          ++
          [ do v <- elements vs
               let vs' = dropWhile (\v' -> compareItem (Proxy :: Proxy a) v' v  /= GT) vs
               p0 <- f vs' (n `div` 2)
               p1 <- f vs' (n `div` 2) `suchThat` (/= Internal.F)
               return (Internal.Branch v p0 p1)
          | n > 0, not (null vs)
          ]
    sized (f vars)

  shrink (ZDD Internal.F) = []
  shrink (ZDD Internal.T) = []
  shrink (ZDD (Internal.Branch x p0 p1)) =
    [ZDD p0, ZDD p1]
    ++
    [ ZDD (Internal.Branch x p0' p1')
    | (ZDD p0', ZDD p1') <- shrink (ZDD p0 :: ZDD a, ZDD p1 :: ZDD a), p1' /= Internal.F
    ]

-- ------------------------------------------------------------------------
-- Union
-- ------------------------------------------------------------------------

prop_union_unitL :: Property
prop_union_unitL =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      ZDD.empty `ZDD.union` a === a

prop_union_unitR :: Property
prop_union_unitR =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      a `ZDD.union` ZDD.empty === a

prop_union_comm :: Property
prop_union_comm =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b) ->
      a `ZDD.union` b === b `ZDD.union` a

prop_union_assoc :: Property
prop_union_assoc =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b, c) ->
      a `ZDD.union` (b `ZDD.union` c) === (a `ZDD.union` b) `ZDD.union` c

prop_union_idempotent :: Property
prop_union_idempotent =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      a `ZDD.union` a === a

-- ------------------------------------------------------------------------
-- Intersection
-- ------------------------------------------------------------------------

prop_intersection_comm :: Property
prop_intersection_comm =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b) ->
      a `ZDD.intersection` b === b `ZDD.intersection` a

prop_intersection_assoc :: Property
prop_intersection_assoc =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b, c) ->
      a `ZDD.intersection` (b `ZDD.intersection` c) === (a `ZDD.intersection` b) `ZDD.intersection` c

prop_intersection_idempotent :: Property
prop_intersection_idempotent =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      a `ZDD.intersection` a === a

prop_intersection_emptyL :: Property
prop_intersection_emptyL =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      ZDD.empty `ZDD.intersection` a === ZDD.empty

prop_intersection_emptyR :: Property
prop_intersection_emptyR =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      a `ZDD.intersection` ZDD.empty === ZDD.empty

prop_intersection_distL :: Property
prop_intersection_distL =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b, c) ->
      a `ZDD.intersection` (b `ZDD.union` c) === (a `ZDD.intersection` b) `ZDD.union` (a `ZDD.intersection` c)

prop_intersection_distR :: Property
prop_intersection_distR =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b, c) ->
      (a `ZDD.union` b) `ZDD.intersection` c === (a `ZDD.intersection` c) `ZDD.union` (b `ZDD.intersection` c)

-- ------------------------------------------------------------------------
-- Difference
-- ------------------------------------------------------------------------

prop_difference_cancel :: Property
prop_difference_cancel =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      a ZDD.\\ a === ZDD.empty

prop_difference_unit :: Property
prop_difference_unit =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      a ZDD.\\ ZDD.empty === a

prop_union_difference :: Property
prop_union_difference =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b, c) ->
      (a `ZDD.union` b) ZDD.\\ c === (a ZDD.\\ c) `ZDD.union` (b ZDD.\\ c)

prop_difference_union :: Property
prop_difference_union =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b, c) ->
      a ZDD.\\ (b `ZDD.union` c) === (a ZDD.\\ b) ZDD.\\ c

-- ------------------------------------------------------------------------
-- Non-superset
-- ------------------------------------------------------------------------

prop_nonSuperset :: Property
prop_nonSuperset =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b) ->
      let a' = ZDD.toSetOfIntSets a
          b' = ZDD.toSetOfIntSets b
          p xs = and [not (IntSet.isSubsetOf ys xs) | ys <- Set.toList b']
       in ZDD.toSetOfIntSets (a `ZDD.nonSuperset` b) === Set.filter p a'

prop_nonSuperset_cancel :: Property
prop_nonSuperset_cancel =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      a `ZDD.nonSuperset` a === ZDD.empty

prop_nonSuperset_unit :: Property
prop_nonSuperset_unit =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      a `ZDD.nonSuperset` ZDD.empty === a

prop_union_nonSuperset :: Property
prop_union_nonSuperset =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b, c) ->
      (a `ZDD.union` b) `ZDD.nonSuperset` c === (a `ZDD.nonSuperset` c) `ZDD.union` (b `ZDD.nonSuperset` c)

prop_nonSuperset_union :: Property
prop_nonSuperset_union =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b, c) ->
      a `ZDD.nonSuperset` (b `ZDD.union` c) === (a `ZDD.nonSuperset` b) `ZDD.nonSuperset` c

prop_nonSuperset_difference :: Property
prop_nonSuperset_difference =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b) ->
      let c = a `ZDD.nonSuperset` b
          d = a ZDD.\\ b
       in counterexample (show (c, d)) $ c `ZDD.isSubsetOf` d

-- ------------------------------------------------------------------------
-- Minimal hitting sets
-- ------------------------------------------------------------------------

isHittingSetOf :: IntSet -> Set IntSet -> Bool
isHittingSetOf s g = all (\e -> not (IntSet.null (s `IntSet.intersection` e))) g

prop_minimalHittingSetsKnuth_isHittingSet :: Property
prop_minimalHittingSetsKnuth_isHittingSet =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      let b = ZDD.minimalHittingSetsKnuth a
          a' = ZDD.toSetOfIntSets a
          b' = ZDD.toSetOfIntSets b
       in all (`isHittingSetOf` a') b'

prop_minimalHittingSetsImai_isHittingSet :: Property
prop_minimalHittingSetsImai_isHittingSet =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      let b = ZDD.minimalHittingSetsImai a
          a' = ZDD.toSetOfIntSets a
          b' = ZDD.toSetOfIntSets b
       in all (`isHittingSetOf` a') b'

prop_minimalHittingSetsToda_isHittingSet :: Property
prop_minimalHittingSetsToda_isHittingSet =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      let b = ZDD.minimalHittingSetsToda a
          a' = ZDD.toSetOfIntSets a
          b' = ZDD.toSetOfIntSets b
       in all (`isHittingSetOf` a') b'

prop_minimalHittingSetsKnuth_duality :: Property
prop_minimalHittingSetsKnuth_duality =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      let b = ZDD.minimalHittingSetsKnuth a
       in ZDD.minimalHittingSetsKnuth (ZDD.minimalHittingSetsKnuth b) === b

prop_minimalHittingSetsImai_duality :: Property
prop_minimalHittingSetsImai_duality =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      let b = ZDD.minimalHittingSetsImai a
       in ZDD.minimalHittingSetsImai (ZDD.minimalHittingSetsImai b) === b

prop_minimalHittingSetsToda_duality :: Property
prop_minimalHittingSetsToda_duality =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      let b = ZDD.minimalHittingSetsToda a
       in ZDD.minimalHittingSetsToda (ZDD.minimalHittingSetsToda b) === b

prop_minimalHittingSets_Imai_equal_Knuth :: Property
prop_minimalHittingSets_Imai_equal_Knuth =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      ZDD.minimalHittingSetsImai a === ZDD.minimalHittingSetsKnuth a

prop_minimalHittingSets_Toda_equal_Knuth :: Property
prop_minimalHittingSets_Toda_equal_Knuth =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      ZDD.minimalHittingSetsToda a === ZDD.minimalHittingSetsKnuth a

-- ------------------------------------------------------------------------
-- Misc
-- ------------------------------------------------------------------------

prop_empty :: Property
prop_empty =
  withDefaultOrder $ \(_ :: Proxy o) ->
    ZDD.toSetOfIntSets (ZDD.empty :: ZDD o) === Set.empty

prop_base :: Property
prop_base =
  withDefaultOrder $ \(_ :: Proxy o) ->
    ZDD.toSetOfIntSets (ZDD.base :: ZDD o) === Set.singleton IntSet.empty

prop_change :: Property
prop_change =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
    forAll arbitrary $ \x ->
      let f xs
            | IntSet.member x xs = IntSet.delete x xs
            | otherwise = IntSet.insert x xs
       in ZDD.toSetOfIntSets (ZDD.change x a) === Set.map f (ZDD.toSetOfIntSets a)

prop_subset1 :: Property
prop_subset1 =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
    forAll arbitrary $ \x ->
      ZDD.toSetOfIntSets (ZDD.subset1 x a) === Set.map (IntSet.delete x) (Set.filter (IntSet.member x) (ZDD.toSetOfIntSets a))

prop_subset0 :: Property
prop_subset0 =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
    forAll arbitrary $ \x ->
      ZDD.toSetOfIntSets (ZDD.subset0 x a) === Set.filter (IntSet.notMember x) (ZDD.toSetOfIntSets a)

prop_size :: Property
prop_size =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      ZDD.size a === Set.size (ZDD.toSetOfIntSets a)

prop_null_size :: Property
prop_null_size =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      ZDD.null a === (ZDD.size a == (0 :: Int))

prop_isSubsetOf_refl :: Property
prop_isSubsetOf_refl =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      a `ZDD.isSubsetOf` a

prop_isSubsetOf_empty :: Property
prop_isSubsetOf_empty =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      ZDD.empty `ZDD.isSubsetOf` a

prop_isSubsetOf_and_isProperSubsetOf :: Property
prop_isSubsetOf_and_isProperSubsetOf =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b) ->
      (a `ZDD.isSubsetOf` b) === (a `ZDD.isProperSubsetOf` b || a == b)

prop_isProperSubsetOf_not_refl :: Property
prop_isProperSubsetOf_not_refl =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o) ->
      not (ZDD.isProperSubsetOf a a)

prop_disjoint :: Property
prop_disjoint =
  withDefaultOrder $ \(_ :: Proxy o) ->
    forAll arbitrary $ \(a :: ZDD o, b) ->
      ZDD.disjoint a b === ZDD.null (a `ZDD.intersection` b)

-- ------------------------------------------------------------------------

zddTestGroup :: TestTree
zddTestGroup = $(testGroupGenerator)
