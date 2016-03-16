{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Map.Class
    ( Map.Map
    , IntMap.IntMap
    , HashMap.HashMap
    , Set.Set
    , IntSet.IntSet
    , HashSet.HashSet
    , MapClass (..)
    , OrdMap (..)
    , PolyMap (..)
    , MapSet (..)
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import qualified Data.Maybe as Maybe
import Prelude hiding (lookup)
import Control.Arrow (first, (&&&), (***))
import Data.Foldable (fold, foldr', foldl')
import Data.Either (partitionEithers)
import qualified Data.List

class Functor map => MapClass k map | map -> k where
    member :: k -> map v -> Bool

    notMember :: k -> map v -> Bool
    notMember k m = not (member k m)
    {-# INLINE notMember #-}

    lookup :: k -> map v -> Maybe v

    findWithDefault :: v -> k -> map v -> v
    findWithDefault v k = Maybe.fromMaybe v . lookup k
    {-# INLINE findWithDefault #-}

    singleton :: k -> v -> map v

    insert :: k -> v -> map v -> map v
    insertWith :: (v -> v -> v) -> k -> v -> map v -> map v
    insertWithKey :: (k -> v -> v -> v) -> k -> v -> map v -> map v
    insertWithKey f k = insertWith (f k) k
    {-# INLINE insertWithKey #-}
    insertLookupWithKey :: (k -> v -> v -> v)
                        -> k
                        -> v
                        -> map v
                        -> (Maybe v, map v)
    insertLookupWithKey f k v m = (lookup k m, insertWithKey f k v m)
    {-# INLINE insertLookupWithKey #-}

    delete :: k -> map v -> map v
    adjust :: (v -> v) -> k -> map v -> map v
    adjustWithKey :: (k -> v -> v) -> k -> map v -> map v
    adjustWithKey f k = adjust (f k) k
    {-# INLINE adjustWithKey #-}
    update :: (v -> Maybe v) -> k -> map v -> map v
    update f = alter (>>= f)
    {-# INLINE update #-}
    updateWithKey :: (k -> v -> Maybe v) -> k -> map v -> map v
    updateWithKey f k = update (f k) k
    {-# INLINE updateWithKey #-}
    updateLookupWithKey :: (k -> v -> Maybe v) -> k -> map v -> (Maybe v, map v)
    updateLookupWithKey f k m = (lookup k m, updateWithKey f k m)
    alter :: (Maybe v -> Maybe v) -> k -> map v -> map v
    alter f k m =
        case f (lookup k m) of
            Nothing -> delete k m
            Just v -> insert k v m

    unionWith :: (v -> v -> v) -> map v -> map v -> map v
    unionWithKey :: (k -> v -> v -> v) -> map v -> map v -> map v
    unionWithKey f m1 m2 =
        mapWithKey f' (unionWith combine m1' m2')
      where
        combine (x, _) (_, y) = (x, y)
        m1' = fmap (\x -> (Just x, Nothing)) m1
        m2' = fmap (\x -> (Just x, Nothing)) m2
        f' _ (Nothing, Nothing) = error "unionWithKey: the impossible happened"
        f' _ (Just x, Nothing) = x
        f' _ (Nothing, Just y) = y
        f' k (Just x, Just y) = f k x y
    unionsWith :: (v -> v -> v) -> [map v] -> map v
    default unionsWith :: Monoid (map v) => (v -> v -> v) -> [map v] -> map v
    unionsWith f = foldr (unionWith f) mempty
    {-# INLINE unionsWith #-}
    unionsWithKey :: (k -> v -> v -> v) -> [map v] -> map v
    default unionsWithKey
        :: Monoid (map v)
        => (k -> v -> v -> v) -> [map v] -> map v
    unionsWithKey f = foldr (unionWithKey f) mempty
    {-# INLINE unionsWithKey #-}

    difference :: map v1 -> map v2 -> map v1
    differenceWith :: (v1 -> v2 -> Maybe v1) -> map v1 -> map v2 -> map v1
    differenceWith f = differenceWithKey (const f)
    {-# INLINE differenceWith #-}
    differenceWithKey :: (k -> v1 -> v2 -> Maybe v1) -> map v1 -> map v2 -> map v1
    default differenceWithKey
        :: Monoid (map v1)
        => (k -> v1 -> v2 -> Maybe v1) -> map v1 -> map v2 -> map v1
    differenceWithKey f = mergeWithKey f id (const mempty)
    {-# INLINE differenceWithKey #-}

    intersection :: map v1 -> map v2 -> map v1
    intersectionWith :: (v1 -> v2 -> v3) -> map v1 -> map v2 -> map v3
    intersectionWithKey :: (k -> v1 -> v2 -> v3) -> map v1 -> map v2 -> map v3
    default intersectionWithKey
        :: Monoid (map v3)
        => (k -> v1 -> v2 -> v3) -> map v1 -> map v2 -> map v3
    intersectionWithKey f =
        mergeWithKey f' (const mempty) (const mempty)
      where
        f' k v1 v2 = Just (f k v1 v2)

    mergeWithKey :: (k -> v1 -> v2 -> Maybe v3)
                 -> (map v1 -> map v3)
                 -> (map v2 -> map v3)
                 -> map v1
                 -> map v2
                 -> map v3

    mapWithKey :: (k -> v1 -> v2) -> map v1 -> map v2
    traverseWithKey :: Applicative t
                    => (k -> v1 -> t v2)
                    -> map v1
                    -> t (map v2)
    traverseWithKey f = fmap fromList
                      . traverse (\(k, v) -> (,) k <$> f k v)
                      . toList

    foldrWithKey :: (k -> v -> a -> a) -> a -> map v -> a
    foldrWithKey f z = foldr (uncurry f) z . toList
    {-# INLINE foldrWithKey #-}
    foldlWithKey :: (a -> k -> v -> a) -> a -> map v -> a
    foldlWithKey f z = foldl (\z' (kx, x) -> f z' kx x) z . toList
    {-# INLINE foldlWithKey #-}
    foldMapWithKey :: Monoid m => (k -> v -> m) -> map v -> m
    default foldMapWithKey
        :: (Foldable map, Monoid m)
        => (k -> v -> m) -> map v -> m
    foldMapWithKey f = fold . mapWithKey f
    {-# INLINE foldMapWithKey #-}
    foldrWithKey' :: (k -> v -> a -> a) -> a -> map v -> a
    foldrWithKey' f z = foldr' (uncurry f) z . toList
    {-# INLINE foldrWithKey' #-}
    foldlWithKey' :: (a -> k -> v -> a) -> a -> map v -> a
    foldlWithKey' f z = foldl' (\z' (kx, x) -> f z' kx x) z . toList
    {-# INLINE foldlWithKey' #-}

    elems :: map v -> [v]
    keys :: map v -> [k]
    assocs :: map v -> [(k, v)]

    toList :: map v -> [(k, v)]
    fromList :: [(k, v)] -> map v
    fromListWith :: (v -> v -> v) -> [(k, v)] -> map v
    fromListWithKey :: (k -> v -> v -> v) -> [(k, v)] -> map v
    fromListWithKey f = unionsWithKey f . map (uncurry singleton)
    {-# INLINE fromListWithKey #-}

    filter :: (v -> Bool) -> map v -> map v
    filterWithKey :: (k -> v -> Bool) -> map v -> map v
    partition :: (v -> Bool) -> map v -> (map v, map v)
    partition f = partitionWithKey (const f)
    {-# INLINE partition #-}
    partitionWithKey :: (k -> v -> Bool) -> map v -> (map v, map v)
    partitionWithKey f =
        (fromList *** fromList) . partitionEithers . map f' . toList
      where
        f' (k, v)
            | f k v = Left (k, v)
            | otherwise = Right (k, v)
    mapMaybe :: (v1 -> Maybe v2) -> map v1 -> map v2
    mapMaybe f = mapMaybeWithKey (const f)
    {-# INLINE mapMaybe #-}
    mapMaybeWithKey :: (k -> v1 -> Maybe v2) -> map v1 -> map v2
    mapMaybeWithKey f =
        fromList . Maybe.mapMaybe f' . toList
      where
        f' (k, v) = fmap (k, ) (f k v)
    mapEither :: (v1 -> Either v2 v3) -> map v1 -> (map v2, map v3)
    mapEither f = mapEitherWithKey (const f)
    {-# INLINE mapEither #-}
    mapEitherWithKey :: (k -> v1 -> Either v2 v3) -> map v1 -> (map v2, map v3)
    mapEitherWithKey f =
        (fromList *** fromList) . partitionEithers . map f' . toList
      where
        f' (k, v) = either (Left . (k, )) (Right . (k, )) (f k v)

instance Ord k => MapClass k (Map.Map k) where
    member = Map.member
    notMember = Map.notMember
    lookup = Map.lookup
    findWithDefault = Map.findWithDefault

    singleton = Map.singleton

    insert = Map.insert
    insertWith = Map.insertWith
    insertWithKey = Map.insertWithKey
    insertLookupWithKey = Map.insertLookupWithKey

    delete = Map.delete
    adjust = Map.adjust
    adjustWithKey = Map.adjustWithKey
    update = Map.update
    updateWithKey = Map.updateWithKey
    updateLookupWithKey = Map.updateLookupWithKey
    alter = Map.alter

    unionWith = Map.unionWith
    unionWithKey = Map.unionWithKey
    unionsWith = Map.unionsWith

    difference = Map.difference
    differenceWith = Map.differenceWith
    differenceWithKey = Map.differenceWithKey

    intersection = Map.intersection
    intersectionWith = Map.intersectionWith
    intersectionWithKey = Map.intersectionWithKey

    mergeWithKey = Map.mergeWithKey

    mapWithKey = Map.mapWithKey
    traverseWithKey = Map.traverseWithKey

    foldrWithKey = Map.foldrWithKey
    foldlWithKey = Map.foldlWithKey
    foldMapWithKey = Map.foldMapWithKey
    foldrWithKey' = Map.foldrWithKey'
    foldlWithKey' = Map.foldlWithKey'

    elems = Map.elems
    keys = Map.keys
    assocs = Map.assocs

    toList = Map.toList
    fromList = Map.fromList
    fromListWith = Map.fromListWith
    fromListWithKey = Map.fromListWithKey

    filter = Map.filter
    filterWithKey = Map.filterWithKey
    partition = Map.partition
    partitionWithKey = Map.partitionWithKey
    mapMaybe = Map.mapMaybe
    mapMaybeWithKey = Map.mapMaybeWithKey
    mapEither = Map.mapEither
    mapEitherWithKey = Map.mapEitherWithKey

instance MapClass Int IntMap.IntMap where
    member = IntMap.member
    notMember = IntMap.notMember
    lookup = IntMap.lookup
    findWithDefault = IntMap.findWithDefault

    singleton = IntMap.singleton

    insert = IntMap.insert
    insertWith = IntMap.insertWith
    insertWithKey = IntMap.insertWithKey
    insertLookupWithKey = IntMap.insertLookupWithKey

    delete = IntMap.delete
    adjust = IntMap.adjust
    adjustWithKey = IntMap.adjustWithKey
    update = IntMap.update
    updateWithKey = IntMap.updateWithKey
    updateLookupWithKey = IntMap.updateLookupWithKey
    alter = IntMap.alter

    unionWith = IntMap.unionWith
    unionWithKey = IntMap.unionWithKey
    unionsWith = IntMap.unionsWith

    difference = IntMap.difference
    differenceWith = IntMap.differenceWith
    differenceWithKey = IntMap.differenceWithKey

    intersection = IntMap.intersection
    intersectionWith = IntMap.intersectionWith
    intersectionWithKey = IntMap.intersectionWithKey

    mergeWithKey = IntMap.mergeWithKey

    mapWithKey = IntMap.mapWithKey
    traverseWithKey = IntMap.traverseWithKey

    foldrWithKey = IntMap.foldrWithKey
    foldlWithKey = IntMap.foldlWithKey
    foldMapWithKey = IntMap.foldMapWithKey
    foldrWithKey' = IntMap.foldrWithKey'
    foldlWithKey' = IntMap.foldlWithKey'

    elems = IntMap.elems
    keys = IntMap.keys
    assocs = IntMap.assocs

    toList = IntMap.toList
    fromList = IntMap.fromList
    fromListWith = IntMap.fromListWith
    fromListWithKey = IntMap.fromListWithKey

    filter = IntMap.filter
    filterWithKey = IntMap.filterWithKey
    partition = IntMap.partition
    partitionWithKey = IntMap.partitionWithKey
    mapMaybe = IntMap.mapMaybe
    mapMaybeWithKey = IntMap.mapMaybeWithKey
    mapEither = IntMap.mapEither
    mapEitherWithKey = IntMap.mapEitherWithKey

instance (Hashable k, Eq k) => MapClass k (HashMap.HashMap k) where
    member = HashMap.member
    lookup = HashMap.lookup

    singleton = HashMap.singleton

    insert = HashMap.insert
    insertWith = HashMap.insertWith

    delete = HashMap.delete
    adjust = HashMap.adjust

    unionWith = HashMap.unionWith

    difference = HashMap.difference

    intersection = HashMap.intersection
    intersectionWith = HashMap.intersectionWith

    mergeWithKey combine fm1 fm2 m1 m2 =
        both `HashMap.union` left `HashMap.union` right
      where
        m1' = fmap (\x -> (Just x, Nothing)) m1
        m2' = fmap (\y -> (Nothing, Just y)) m2

        full = HashMap.unionWith (\(x, _) (_, y) -> (x, y)) m1' m2'
        list = HashMap.toList full

        both = HashMap.fromList $ Maybe.mapMaybe (\(k, pair) ->
            case pair of
                (Just x, Just y) -> fmap (k, ) (combine k x y)
                _ -> Nothing) list

        left = fm1 $ HashMap.fromList $ Maybe.mapMaybe (\(k, pair) ->
            case pair of
                (Just x, Nothing) -> Just (k, x)
                _ -> Nothing) list

        right = fm2 $ HashMap.fromList $ Maybe.mapMaybe (\(k, pair) ->
            case pair of
                (Nothing, Just y) -> Just (k, y)
                _ -> Nothing) list

    mapWithKey = HashMap.mapWithKey
    traverseWithKey = HashMap.traverseWithKey

    foldrWithKey = HashMap.foldrWithKey
    foldlWithKey' = HashMap.foldlWithKey'

    elems = HashMap.elems
    keys = HashMap.keys
    assocs = HashMap.toList

    toList = HashMap.toList
    fromList = HashMap.fromList
    fromListWith = HashMap.fromListWith

    filter = HashMap.filter
    filterWithKey = HashMap.filterWithKey

class MapClass k map => OrdMap k map | map -> k where
    lookupLT :: k -> map v -> Maybe (k, v)
    lookupGT :: k -> map v -> Maybe (k, v)
    lookupLE :: k -> map v -> Maybe (k, v)
    lookupGE :: k -> map v -> Maybe (k, v)

    mapAccum :: (accum -> v1 -> (accum, v2))
             -> accum
             -> map v1
             -> (accum, map v2)
    mapAccum f = mapAccumWithKey (\accum _ -> f accum)
    {-# INLINE mapAccum #-}
    mapAccumWithKey
        :: (accum -> k -> v1 -> (accum, v2))
        -> accum
        -> map v1
        -> (accum, map v2)
    mapAccumRWithKey
        :: (accum -> k -> v1 -> (accum, v2))
        -> accum
        -> map v1
        -> (accum, map v2)

    toAscList :: map v -> [(k, v)]
    toDescList :: map v -> [(k, v)]
    fromAscList :: [(k, v)] -> map v
    fromAscListWith :: (v -> v -> v) -> [(k, v)] -> map v
    fromAscListWithKey :: (k -> v -> v -> v) -> [(k, v)] -> map v
    fromDistinctAscList :: [(k, v)] -> map v

    split :: k -> map v -> (map v, map v)
    splitLookup :: k -> map v -> (map v, Maybe v, map v)
    splitRoot :: map v -> [map v]
instance Ord k => OrdMap k (Map.Map k) where
    lookupLT = Map.lookupLT
    lookupGT = Map.lookupGT
    lookupLE = Map.lookupLE
    lookupGE = Map.lookupGE

    mapAccum = Map.mapAccum
    mapAccumWithKey = Map.mapAccumWithKey
    mapAccumRWithKey = Map.mapAccumRWithKey

    toAscList = Map.toAscList
    toDescList = Map.toDescList
    fromAscList = Map.fromAscList
    fromAscListWith = Map.fromAscListWith
    fromAscListWithKey = Map.fromAscListWithKey
    fromDistinctAscList = Map.fromDistinctAscList

    split = Map.split
    splitLookup = Map.splitLookup
    splitRoot = Map.splitRoot
instance OrdMap Int IntMap.IntMap where
    lookupLT = IntMap.lookupLT
    lookupGT = IntMap.lookupGT
    lookupLE = IntMap.lookupLE
    lookupGE = IntMap.lookupGE

    mapAccum = IntMap.mapAccum
    mapAccumWithKey = IntMap.mapAccumWithKey
    mapAccumRWithKey = IntMap.mapAccumRWithKey

    toAscList = IntMap.toAscList
    toDescList = IntMap.toDescList
    fromAscList = IntMap.fromAscList
    fromAscListWith = IntMap.fromAscListWith
    fromAscListWithKey = IntMap.fromAscListWithKey
    fromDistinctAscList = IntMap.fromDistinctAscList

    split = IntMap.split
    splitLookup = IntMap.splitLookup
    splitRoot = IntMap.splitRoot

class (MapClass k1 map1, MapClass k2 map2) => PolyMap k1 map1 k2 map2 where
    mapKeys :: (k1 -> k2) -> map1 v -> map2 v
    mapKeys = mapKeysWith const
    {-# INLINE mapKeys #-}
    mapKeysWith :: (v -> v -> v) -> (k1 -> k2) -> map1 v -> map2 v
    mapKeysWith fv fk = fromListWith fv . map (first fk) . toList
    {-# INLINE mapKeysWith #-}

    mapKeysMonotonic :: (k1 -> k2) -> map1 v -> map2 v
    mapKeysMonotonic = mapKeys
    {-# INLINE mapKeysMonotonic #-}

instance (Ord k1, Ord k2) => PolyMap k1 (Map.Map k1) k2 (Map.Map k2) where
    mapKeys = Map.mapKeys
    mapKeysWith = Map.mapKeysWith
    mapKeysMonotonic = Map.mapKeysMonotonic

instance (Hashable k1, Eq k1, Hashable k2, Eq k2)
  => PolyMap k1 (HashMap.HashMap k1) k2 (HashMap.HashMap k2) where

class MapClass k map => MapSet k map set | map -> k set where
    keysSet :: map v -> set
    fromSet :: (k -> v) -> set -> map v

instance Ord k => MapSet k (Map.Map k) (Set.Set k) where
    keysSet = Map.keysSet
    fromSet = Map.fromSet

instance MapSet Int IntMap.IntMap IntSet.IntSet where
    keysSet = IntMap.keysSet
    fromSet = IntMap.fromSet

instance (Hashable k, Eq k) => MapSet k (HashMap.HashMap k) (HashSet.HashSet k) where
    keysSet = HashSet.fromList . keys
    fromSet f = fromList . map (id &&& f) . HashSet.toList
