module Combinators where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Bool
import Data.Foldable
import Data.Function
import Data.Int
import Data.Monoid
import Data.Traversable
import Data.Tuple
import GHC.Enum

-- * Alternation

-- |
-- Generalization over many common natural transformations, including:
--
-- - 'listToMaybe'
-- - 'maybeToList'
-- - 'toList'
-- - @'either' ('const' 'Nothing') 'Just'@
{-# INLINE alternate #-}
alternate :: (Foldable f, Alternative g) => f a -> g a
alternate = alternateMapM pure

-- |
-- 'alternate' extended with ability to map the wrapped value.
{-# INLINE alternateMap #-}
alternateMap :: (Foldable f, Alternative g) => (a -> b) -> f a -> g b
alternateMap mapper = alternateMapM (pure . mapper)

-- |
-- 'alternateMap' extended with ability to do the mapping in the 'Alternative' context.
{-# INLINE alternateMapM #-}
alternateMapM :: (Foldable f, Alternative g) => (a -> g b) -> f a -> g b
alternateMapM mapper = foldr cons empty
  where
    cons a b = mapper a <|> b

-- * Folding

-- |
-- A generic version of the original list-specialized version:
--
-- > intercalate :: [a] -> [[a]] -> [a]
{-# INLINE intercalate #-}
intercalate :: (Foldable f, Monoid a) => a -> f a -> a
intercalate = flip intercalateMap id

-- |
-- 'intercalate' extended with ability to map the wrapped value.
{-# INLINE intercalateMap #-}
intercalateMap :: (Foldable f, Monoid m) => m -> (a -> m) -> f a -> m
intercalateMap separator proj =
  fst
    . foldl'
      ( \(acc, isFirst) element ->
          if isFirst
            then (proj element, False)
            else (acc <> separator <> proj element, False)
      )
      (mempty, True)

-- * Traversal

-- |
-- Indexed version of 'forM'.
{-# INLINE iforM #-}
iforM :: (Traversable f, Monad m) => f a -> (Int -> a -> m b) -> m (f b)
iforM collection f =
  collection
    & traverse
      ( \item -> do
          i <- state (\i -> (i, succ i))
          lift (f i item)
      )
    & flip evalStateT 0
