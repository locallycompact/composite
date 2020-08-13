{-# LANGUAGE UndecidableInstances #-}
module Composite.Record.Binary where

import Composite.Record((:->), Record, pattern (:*:), val, getVal)
import Control.Applicative(liftA2)
import Data.Binary(Binary(put, get))
import Data.Functor.Identity(runIdentity)

instance Binary a => Binary (s :-> a) where
  put = put . getVal
  get = fmap (runIdentity . val) get

instance Binary (Record '[])

instance (Binary a, Binary (Record xs), x ~ (s :-> a)) => Binary (Record (x : xs)) where
  put (x :*: xs) = put x >> put xs
  get = liftA2 (:*:) get get
