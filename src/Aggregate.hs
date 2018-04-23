{-# LANGUAGE MultiParamTypeClasses #-}

module Aggregate where

import           Data.Foldable      (foldl')
import           Data.Time          (UTCTime (..), secondsToDiffTime)
import           Data.Time.Calendar (Day (..))

-- | Types

-- | A unique ID for an aggregate. This ID is assigned to all
type AggregateId = String

-- | Classes

-- | zero is an initial state of an aggregate before any events have occurred.
class Zero a where
    zero :: a

instance Zero UTCTime where
  zero = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

-- | Represents the relationship between event e and state s.
class (Zero s) => Aggregate e s where
    applyEvent :: s -> e -> s

    foldEvents :: s -> [e] -> s
    foldEvents = foldl' applyEvent

-- | Represents a command that can be executed on a state to render events.
class (Aggregate e s) => Command s e c where
    exec :: s -> c -> Either String [e]
