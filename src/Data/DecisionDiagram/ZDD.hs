{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.DecisionDiagram.ZDD
-- Copyright   :  (c) Masahiro Sakai 2021
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  unstable
-- Portability :  non-portable
--
-- Zero-Suppressed binary decision diagram.
--
-- References:
--
-- * S. Minato, "Zero-Suppressed BDDs for Set Manipulation in Combinatorial Problems,"
--   30th ACM/IEEE Design Automation Conference, 1993, pp. 272-277,
--   doi: [10.1145/157485.164890](https://doi.org/10.1145/157485.164890).
--   <https://www.researchgate.net/publication/221062015_Zero-Suppressed_BDDs_for_Set_Manipulation_in_Combinatorial_Problems>
--
----------------------------------------------------------------------
module Data.DecisionDiagram.ZDD
  (
  -- * ZDD type
    ZDD (..)

  -- * Item ordering
  , ItemOrder (..)
  , DefaultOrder
  , withDefaultOrder
  , withCustomOrder

  -- * Construction
  , empty
  , base
  , singleton
  , fromListOfIntSets
  , fromSetOfIntSets

  -- * Insertion
  , insert

  -- * Deletion
  , delete

  -- * Query
  , member
  , notMember
  , null
  , size
  , isSubsetOf
  , isProperSubsetOf
  , disjoint

  -- * Combine
  , union
  , unions
  , intersection
  , difference
  , (\\)
  , nonSuperset

  -- * Filter
  , subset1
  , subset0

  -- * Map
  , mapInsert
  , mapDelete
  , change

  -- * Fold
  , fold
  , fold'

  -- * Minimal hitting sets
  , minimalHittingSets
  , minimalHittingSetsToda
  , minimalHittingSetsKnuth
  , minimalHittingSetsImai

  -- * Random sampling
  , uniformM

  -- * Misc
  , flatten

  -- * Conversion
  , toListOfIntSets
  , toSetOfIntSets
  ) where

import Prelude hiding (null)

import Control.Monad
#if !MIN_VERSION_mwc_random(0,15,0)
import Control.Monad.Primitive
#endif
import Control.Monad.ST
import qualified Data.Foldable as Foldable
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (sortBy)
import Data.Proxy
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as Set
import qualified GHC.Exts as Exts
import Numeric.Natural
#if MIN_VERSION_mwc_random(0,15,0)
import System.Random.Stateful (StatefulGen (..))
#else
import System.Random.MWC (Gen)
#endif
import System.Random.MWC.Distributions (bernoulli)

import Data.DecisionDiagram.BDD.Internal.ItemOrder
import Data.DecisionDiagram.BDD.Internal.Node
import qualified Data.DecisionDiagram.BDD as BDD

-- ------------------------------------------------------------------------

defaultTableSize :: Int
defaultTableSize = 256

-- ------------------------------------------------------------------------

-- | Zero-suppressed binary decision diagram representing family of sets
newtype ZDD a = ZDD Node
  deriving (Eq, Hashable, Show)

instance ItemOrder a => Exts.IsList (ZDD a) where
  type Item (ZDD a) = IntSet

  fromList = fromListOfSortedList . map f
    where
      f :: IntSet -> [Int]
      f = sortBy (compareItem (Proxy :: Proxy a)) . IntSet.toList

  toList = fold' [] [IntSet.empty] (\top lo hi -> lo <> map (IntSet.insert top) hi)

zddNode :: Int -> Node -> Node -> Node
zddNode _ p0 F = p0
zddNode top p0 p1 = Branch top p0 p1

data ZDDCase2Node
  = ZDDCase2LT Int Node Node
  | ZDDCase2GT Int Node Node
  | ZDDCase2EQ Int Node Node Node Node

zddCase2Node :: forall a. ItemOrder a => Proxy a -> Node -> Node -> ZDDCase2Node
zddCase2Node _ (Branch ptop p0 p1) (Branch qtop q0 q1) =
  case compareItem (Proxy :: Proxy a) ptop qtop of
    LT -> ZDDCase2LT ptop p0 p1
    GT -> ZDDCase2GT qtop q0 q1
    EQ -> ZDDCase2EQ ptop p0 p1 q0 q1
zddCase2Node _ (Branch ptop p0 p1) _ = ZDDCase2LT ptop p0 p1
zddCase2Node _ _ (Branch qtop q0 q1) = ZDDCase2GT qtop q0 q1
zddCase2Node _ _ _ = error "should not happen"

-- | The empty set (∅).
empty :: ZDD a
empty = ZDD F

-- | The set containing only the empty set ({∅}).
base :: ZDD a
base = ZDD T

-- | Create a ZDD that contains only a given set.
singleton :: forall a. ItemOrder a => IntSet -> ZDD a
singleton xs = insert xs empty

-- | Select subsets that contain a particular element and then remove the element from them
subset1 :: forall a. ItemOrder a => Int -> ZDD a -> ZDD a
subset1 var (ZDD node) = runST $ do
  h <- C.newSized defaultTableSize
  let f T = return F
      f F = return F
      f p@(Branch top p0 p1) = do
        m <- H.lookup h p
        case m of
          Just ret -> return ret
          Nothing -> do
            ret <- case compareItem (Proxy :: Proxy a) top var of
              GT -> return F
              EQ -> return p1
              LT -> liftM2 (zddNode top) (f p0) (f p1)
            H.insert h p ret
            return ret
  ret <- f node
  return (ZDD ret)

-- | Subsets that does not contain a particular element
subset0 :: forall a. ItemOrder a => Int -> ZDD a -> ZDD a
subset0 var (ZDD node) = runST $ do
  h <- C.newSized defaultTableSize
  let f p@T = return p
      f F = return F
      f p@(Branch top p0 p1) = do
        m <- H.lookup h p
        case m of
          Just ret -> return ret
          Nothing -> do
            ret <- case compareItem (Proxy :: Proxy a) top var of
              GT -> return p
              EQ -> return p0
              LT -> liftM2 (zddNode top) (f p0) (f p1)
            H.insert h p ret
            return ret
  ret <- f node
  return (ZDD ret)

-- | Insert a set into the ZDD.
insert :: forall a. ItemOrder a => IntSet -> ZDD a -> ZDD a
insert xs (ZDD node) = ZDD $ f (sortBy (compareItem (Proxy :: Proxy a)) (IntSet.toList xs)) node
  where
    f [] F = T
    f [] T = T
    f [] (Branch top p0 p1) = zddNode top (f [] p0) p1
    f (y : ys) F = zddNode y F (f ys F)
    f (y : ys) T = zddNode y T (f ys F)
    f yys@(y : ys) p@(Branch top p0 p1) =
      case compareItem (Proxy :: Proxy a) y top of
        LT -> zddNode y p (f ys F)
        GT -> zddNode top (f yys p0) p1
        EQ -> zddNode top p0 (f ys p1)

-- | Delete a set from the ZDD.
delete :: forall a. ItemOrder a => IntSet -> ZDD a -> ZDD a
delete xs (ZDD node) = ZDD $ f (sortBy (compareItem (Proxy :: Proxy a)) (IntSet.toList xs)) node
  where
    f [] F = F
    f [] T = F
    f [] (Branch top p0 p1) = zddNode top (f [] p0) p1
    f (_ : _) F = F
    f (_ : _) T = T
    f yys@(y : ys) p@(Branch top p0 p1) =
      case compareItem (Proxy :: Proxy a) y top of
        LT -> p
        GT -> zddNode top (f yys p0) p1
        EQ -> zddNode top p0 (f ys p1)

-- | Insert an item into each element set of ZDD.
mapInsert :: forall a. ItemOrder a => Int -> ZDD a -> ZDD a
mapInsert var (ZDD node) = runST $ do
  h <- C.newSized defaultTableSize
  let f p@T = return (zddNode var F p)
      f F = return F
      f p@(Branch top p0 p1) = do
        m <- H.lookup h p
        case m of
          Just ret -> return ret
          Nothing -> do
            ret <- case compareItem (Proxy :: Proxy a) top var of
              GT -> return (zddNode var F p)
              LT -> liftM2 (zddNode top) (f p0) (f p1)
              EQ ->
                let ZDD r :: ZDD a = ZDD p0 `union` ZDD p1
                 in return (zddNode top F r)
            H.insert h p ret
            return ret
  ret <- f node
  return (ZDD ret)

-- | Delete an item from each element set of ZDD.
mapDelete :: forall a. ItemOrder a => Int -> ZDD a -> ZDD a
mapDelete var (ZDD node) = runST $ do
  h <- C.newSized defaultTableSize
  let f T = return T
      f F = return F
      f p@(Branch top p0 p1) = do
        m <- H.lookup h p
        case m of
          Just ret -> return ret
          Nothing -> do
            ret <- case compareItem (Proxy :: Proxy a) top var of
              GT -> return p
              LT -> liftM2 (zddNode top) (f p0) (f p1)
              EQ ->
                let ZDD r :: ZDD a = ZDD p0 `union` ZDD p1
                 in return r
            H.insert h p ret
            return ret
  ret <- f node
  return (ZDD ret)

-- | @change x p@ returns {if x∈s then s∖{x} else s∪{x} | s∈P}
change :: forall a. ItemOrder a => Int -> ZDD a -> ZDD a
change var (ZDD node) = runST $ do
  h <- C.newSized defaultTableSize
  let f p@T = return (zddNode var F p)
      f F = return F
      f p@(Branch top p0 p1) = do
        m <- H.lookup h p
        case m of
          Just ret -> return ret
          Nothing -> do
            ret <- case compareItem (Proxy :: Proxy a) top var of
              GT -> return (zddNode var F p)
              EQ -> return (zddNode var p1 p0)
              LT -> liftM2 (zddNode top) (f p0) (f p1)
            H.insert h p ret
            return ret
  ret <- f node
  return (ZDD ret)

-- | Union of two family of sets.
union :: forall a. ItemOrder a => ZDD a -> ZDD a -> ZDD a
union (ZDD node1) (ZDD node2) = runST $ do
  h <- C.newSized defaultTableSize
  let f F q = return q
      f p F = return p
      f p q | p == q = return p
      f p q = do
        let key = if nodeId p <= nodeId q then (p, q) else (q, p)
        m <- H.lookup h key
        case m of
          Just ret -> return ret
          Nothing -> do
            ret <- case zddCase2Node (Proxy :: Proxy a) p q of
              ZDDCase2LT ptop p0 p1 -> liftM2 (zddNode ptop) (f p0 q) (pure p1)
              ZDDCase2GT qtop q0 q1 -> liftM2 (zddNode qtop) (f p q0) (pure q1)
              ZDDCase2EQ top p0 p1 q0 q1 -> liftM2 (zddNode top) (f p0 q0) (f p1 q1)
            H.insert h key ret
            return ret
  ret <- f node1 node2
  return (ZDD ret)

-- | Unions of a list of ZDDs.
unions :: forall f a. (Foldable f, ItemOrder a) => f (ZDD a) -> ZDD a
unions xs = Foldable.foldl' union empty xs

-- | Intersection of two family of sets.
intersection :: forall a. ItemOrder a => ZDD a -> ZDD a -> ZDD a
intersection (ZDD node1) (ZDD node2) = runST $ do
  h <- C.newSized defaultTableSize
  let f F _q = return F
      f _p F = return F
      f p q | p == q = return p
      f p q = do
        let key = if nodeId p <= nodeId q then (p, q) else (q, p)
        m <- H.lookup h key
        case m of
          Just ret -> return ret
          Nothing -> do
            ret <- case zddCase2Node (Proxy :: Proxy a) p q of
              ZDDCase2LT _ptop p0 _p1 -> f p0 q
              ZDDCase2GT _qtop q0 _q1 -> f p q0
              ZDDCase2EQ top p0 p1 q0 q1 -> liftM2 (zddNode top) (f p0 q0) (f p1 q1)
            H.insert h key ret
            return ret
  ret <- f node1 node2
  return (ZDD ret)

-- | Difference of two family of sets.
difference :: forall a. ItemOrder a => ZDD a -> ZDD a -> ZDD a
difference (ZDD node1) (ZDD node2) = runST $ do
  h <- C.newSized defaultTableSize
  let f F _ = return F
      f p F = return p
      f p q | p == q = return F
      f p q = do
        m <- H.lookup h (p, q)
        case m of
          Just ret -> return ret
          Nothing -> do
            ret <- case zddCase2Node (Proxy :: Proxy a) p q of
              ZDDCase2LT ptop p0 p1 -> liftM2 (zddNode ptop) (f p0 q) (pure p1)
              ZDDCase2GT _qtop q0 _q1 -> f p q0
              ZDDCase2EQ top p0 p1 q0 q1 -> liftM2 (zddNode top) (f p0 q0) (f p1 q1)
            H.insert h (p, q) ret
            return ret
  ret <- f node1 node2
  return (ZDD ret)

-- | See 'difference'
(\\) :: forall a. ItemOrder a => ZDD a -> ZDD a -> ZDD a
m1 \\ m2 = difference m1 m2

-- | Given a family P and Q, it computes {S∈P | ∀X∈Q. X⊈S}
--
-- Sometimes it is denoted as /P ↘ Q/.
nonSuperset :: forall a. ItemOrder a => ZDD a -> ZDD a -> ZDD a
nonSuperset (ZDD node1) (ZDD node2) = runST $ do
  h <- C.newSized defaultTableSize
  let f F _ = return F
      f _ T = return F
      f p F = return p
      f p q | p == q = return F
      f p q = do
        m <- H.lookup h (p, q)
        case m of
          Just ret -> return ret
          Nothing -> do
            ret <- case zddCase2Node (Proxy :: Proxy a) p q of
              ZDDCase2LT ptop p0 p1 -> liftM2 (zddNode ptop) (f p0 q) (f p1 q)
              ZDDCase2GT _qtop q0 _q1 -> f p q0
              ZDDCase2EQ top p0 p1 q0 q1 -> do
                n0 <- f p1 q0
                n1 <- f p1 q1
                let ZDD r = intersection (ZDD n0 :: ZDD a) (ZDD n1) -- TODO: memoize intersection?
                liftM2 (zddNode top) (f p0 q0) (pure r)
            H.insert h (p, q) ret
            return ret
  ret <- f node1 node2
  return (ZDD ret)

minimalHittingSetsKnuth' :: forall a. ItemOrder a => Bool -> ZDD a -> ZDD a
minimalHittingSetsKnuth' imai (ZDD node) = runST $ do
  h <- C.newSized defaultTableSize
  let f F = return T
      f T = return F
      f p@(Branch top p0 p1) = do
        m <- H.lookup h p
        case m of
          Just ret -> return ret
          Nothing -> do
            -- TODO: memoize union and difference/nonSuperset?
            r0 <- case union (ZDD p0) (ZDD p1) :: ZDD a of
                    ZDD r -> f r
            ZDD r1 <- liftM2 (if imai then difference else nonSuperset) (liftM ZDD (f p0)) (pure (ZDD r0 :: ZDD a))
            let ret = zddNode top r0 r1
            H.insert h p ret
            return ret
  ret <- f node
  return (ZDD ret)

-- | Minimal hitting sets.
--
-- D. E. Knuth, "The Art of Computer Programming, Volume 4A:
-- Combinatorial Algorithms, Part 1," Addison-Wesley Professional,
-- 2011.
minimalHittingSetsKnuth :: forall a. ItemOrder a => ZDD a -> ZDD a
minimalHittingSetsKnuth = minimalHittingSetsKnuth' False

-- | Minimal hitting sets.
--
-- T. Imai, "One-line hack of knuth's algorithm for minimal hitting set
-- computation with ZDDs," vol. 2015-AL-155, no. 15, Nov. 2015, pp. 1-3.
-- [Online]. Available: <http://id.nii.ac.jp/1001/00145799/>.
minimalHittingSetsImai :: forall a. ItemOrder a => ZDD a -> ZDD a
minimalHittingSetsImai = minimalHittingSetsKnuth' True

-- | Minimal hitting sets.
--
-- * T. Toda, “Hypergraph Transversal Computation with Binary Decision Diagrams,”
--   SEA 2013: Experimental Algorithms.
--   Available: <http://dx.doi.org/10.1007/978-3-642-38527-8_10>.
--
-- * HTC-BDD: Hypergraph Transversal Computation with Binary Decision Diagrams
--   <https://www.disc.lab.uec.ac.jp/toda/htcbdd.html>
minimalHittingSetsToda :: forall a. ItemOrder a => ZDD a -> ZDD a
minimalHittingSetsToda = minimal . hittingSetsBDD

hittingSetsBDD :: forall a. ItemOrder a => ZDD a -> BDD.BDD a
hittingSetsBDD = fold' BDD.true BDD.false (\top h0 h1 -> h0 BDD..&&. bddNode top h1 BDD.true)
  where
    -- XXX
    bddNode :: Int -> BDD.BDD a -> BDD.BDD a -> BDD.BDD a
    bddNode ind (BDD.BDD lo) (BDD.BDD hi)
      | lo == hi = BDD.BDD lo
      | otherwise = BDD.BDD (Branch ind lo hi)

minimal :: forall a. ItemOrder a => BDD.BDD a -> ZDD a
minimal (BDD.BDD node) = runST $ do
  h <- C.newSized defaultTableSize
  let f F = return F
      f T = return T
      f p@(Branch x lo hi) = do
        m <- H.lookup h p
        case m of
          Just ret -> return ret
          Nothing -> do
            ml <- f lo
            mh <- f hi
            let ZDD t = difference (ZDD mh :: ZDD a) (ZDD ml)
                ret = zddNode x ml t
            H.insert h p ret
            return ret
  ret <- f node
  return (ZDD ret)

-- | See 'minimalHittingSetsToda'.
minimalHittingSets :: forall a. ItemOrder a => ZDD a -> ZDD a
minimalHittingSets = minimalHittingSetsToda

-- | Is the set a member of the family?
member :: forall a. (ItemOrder a) => IntSet -> ZDD a -> Bool
member xs zdd = member' xs' zdd
  where
    xs' = sortBy (compareItem (Proxy :: Proxy a)) $ IntSet.toList xs

member' :: forall a. (ItemOrder a) => [Int] -> ZDD a -> Bool
member' xs (ZDD node) = f xs node
  where
    f [] T = True
    f [] (Branch _ p0 _) = f [] p0
    f yys@(y:ys) (Branch top p0 p1) =
      case compareItem (Proxy :: Proxy a) y top of
        EQ -> f ys p1
        GT -> f yys p0
        LT -> False
    f _ _ = False

-- | Is the set not in the family?
notMember :: forall a. (ItemOrder a) => IntSet -> ZDD a -> Bool
notMember xs zdd = not (member xs zdd)

-- | Is this the empty set?
null :: ZDD a -> Bool
null = (empty ==)

{-# SPECIALIZE size :: ZDD a -> Int #-}
{-# SPECIALIZE size :: ZDD a -> Integer #-}
{-# SPECIALIZE size :: ZDD a -> Natural #-}
-- | The number of sets in the family.
size :: (Integral b) => ZDD a -> b
size = fold' 0 1 (\_ n0 n1 -> n0 + n1)

-- | @(s1 `isSubsetOf` s2)@ indicates whether @s1@ is a subset of @s2@.
isSubsetOf :: ItemOrder a => ZDD a -> ZDD a -> Bool
isSubsetOf a b = union a b == b

-- | @(s1 `isProperSubsetOf` s2)@ indicates whether @s1@ is a proper subset of @s2@.
isProperSubsetOf :: ItemOrder a => ZDD a -> ZDD a -> Bool
isProperSubsetOf a b = a `isSubsetOf` b && a /= b

-- | Check whether two sets are disjoint (i.e., their intersection is empty).
disjoint :: ItemOrder a => ZDD a -> ZDD a -> Bool
disjoint a b = null (a `intersection` b)

--- | Unions of all member sets
flatten :: ItemOrder a => ZDD a -> IntSet
flatten = fold' IntSet.empty IntSet.empty (\top lo hi -> IntSet.insert top (lo `IntSet.union` hi))

-- | Create a ZDD from a set of 'IntSet'
fromSetOfIntSets :: forall a. ItemOrder a => Set IntSet -> ZDD a
fromSetOfIntSets = fromListOfIntSets . Set.toList

-- | Convert the family to a set of 'IntSet'.
toSetOfIntSets :: ZDD a -> Set IntSet
toSetOfIntSets = fold' Set.empty (Set.singleton IntSet.empty) (\top lo hi -> lo <> Set.map (IntSet.insert top) hi)

-- | Create a ZDD from a list of 'IntSet'
fromListOfIntSets :: forall a. ItemOrder a => [IntSet] -> ZDD a
fromListOfIntSets = fromListOfSortedList . map f
  where
    f :: IntSet -> [Int]
    f = sortBy (compareItem (Proxy :: Proxy a)) . IntSet.toList

-- | Convert the family to a list of 'IntSet'.
toListOfIntSets :: ZDD a -> [IntSet]
toListOfIntSets = fold [] [IntSet.empty] (\top lo hi -> lo <> map (IntSet.insert top) hi)

fromListOfSortedList :: forall a. ItemOrder a => [[Int]] -> ZDD a
fromListOfSortedList = unions . map f
  where
    f :: [Int] -> ZDD a
    f = ZDD . foldr (\x node -> Branch x F node) T

-- | Fold over the graph structure of the ZDD.
--
-- It takes values for substituting 'empty' and 'base',
-- and a function for substiting non-terminal node.
fold :: b -> b -> (Int -> b -> b -> b) -> ZDD a -> b
fold ff tt br (ZDD node) = runST $ do
  h <- C.newSized defaultTableSize
  let f F = return ff
      f T = return tt
      f p@(Branch top p0 p1) = do
        m <- H.lookup h p
        case m of
          Just ret -> return ret
          Nothing -> do
            r0 <- f p0
            r1 <- f p1
            let ret = br top r0 r1
            H.insert h p ret
            return ret
  f node

-- | Strict version of 'fold'
fold' :: b -> b -> (Int -> b -> b -> b) -> ZDD a -> b
fold' !ff !tt br (ZDD node) = runST $ do
  h <- C.newSized defaultTableSize
  let f F = return ff
      f T = return tt
      f p@(Branch top p0 p1) = do
        m <- H.lookup h p
        case m of
          Just ret -> return ret
          Nothing -> do
            r0 <- f p0
            r1 <- f p1
            let ret = br top r0 r1
            seq ret $ H.insert h p ret
            return ret
  f node

-- ------------------------------------------------------------------------

-- | Sample a set from uniform distribution over elements of the ZDD.
--
-- The function constructs a table internally and the table is shared across
-- multiple use of the resulting action (@m IntSet@).
-- Therefore, the code
--
-- @
-- let g = uniformM zdd gen
-- s1 <- g
-- s2 <- g
-- @
--
-- is more efficient than
--
-- @
-- s1 <- uniformM zdd gen
-- s2 <- uniformM zdd gen
-- @
-- .
#if MIN_VERSION_mwc_random(0,15,0)
uniformM :: forall a g m. (ItemOrder a, StatefulGen g m) => ZDD a -> g -> m IntSet
#else
uniformM :: forall a m. (ItemOrder a, PrimMonad m) => ZDD a -> Gen (PrimState m) -> m IntSet
#endif
uniformM (ZDD F) = error "Data.DecisionDiagram.ZDD.uniformM: empty ZDD"
uniformM (ZDD node) = func
  where
    func gen = f node []
      where
        f F _ = error "Data.DecisionDiagram.ZDD.uniformM: should not happen"
        f T r = return $ IntSet.fromList r
        f p@(Branch top p0 p1) r = do
          b <- bernoulli (table HashMap.! p) gen
          if b then
            f p1 (top : r)
          else
            f p0 r

    table :: HashMap Node Double
    table = runST $ do
      h <- C.newSized defaultTableSize
      let f F = return (0 :: Integer)
          f T = return 1
          f p@(Branch _ p0 p1) = do
            m <- H.lookup h p
            case m of
              Just (ret, _) -> return ret
              Nothing -> do
                n0 <- f p0
                n1 <- f p1
                let s = n0 + n1
                    r :: Double
                    r = realToFrac (n1 % (n0 + n1))
                seq r $ H.insert h p (s, r)
                return s
      _ <- f node
      xs <- H.toList h
      return $ HashMap.fromList [(n, r) | (n, (_, r)) <- xs]

-- ------------------------------------------------------------------------
