module Combinators where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Enum
import Data.Foldable
import Data.Function
import Data.Int
import Data.Traversable

-- | Generalization of 'listToMaybe', 'maybeToList', 'toList' and many others.
{-# INLINE alternate #-}
alternate :: (Foldable f, Alternative g) => f a -> g a
alternate = alternateMapM pure

{-# INLINE alternateMap #-}
alternateMap :: (Foldable f, Alternative g) => (a -> b) -> f a -> g b
alternateMap mapper = alternateMapM (pure . mapper)

{-# INLINE alternateMapM #-}
alternateMapM :: (Foldable f, Alternative g) => (a -> g b) -> f a -> g b
alternateMapM mapper = foldr cons empty
  where
    cons a b = mapper a <|> b

{-# INLINE iforM #-}
iforM :: (Monad m, Traversable f) => f a -> (Int -> a -> m b) -> m (f b)
iforM collection f =
  collection
    & traverse
      ( \item -> do
          i <- state (\i -> (i, succ i))
          lift (f i item)
      )
    & flip evalStateT 0