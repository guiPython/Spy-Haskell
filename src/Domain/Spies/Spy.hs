{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Domain.Spies.Spy where
import Data.Kind (Type)

data Spy = Spy {
  name :: String,
  age  :: Int
} deriving  Show

