{-# LANGUAGE UndecidableInstances #-}
module Composite.Record.Hashable where

import Composite.Record((:->), Record, Rec(RNil), pattern (:*:), val, getVal)
import Data.Hashable(Hashable(hashWithSalt))

instance Hashable a => Hashable (s :-> a) where
  hashWithSalt n x = hashWithSalt n $ getVal x

instance Hashable (Record '[]) where
  hashWithSalt n RNil = n `hashWithSalt` ()

instance (Hashable a, Hashable (Record xs), x ~ (s :-> a)) => Hashable (Record (x : xs)) where
  hashWithSalt n (x :*: xs) = n `hashWithSalt` x `hashWithSalt` xs
