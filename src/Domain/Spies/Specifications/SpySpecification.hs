{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}


module Domain.Spies.Specifications.SpySpecification where
import Data.Kind ( Type, Constraint )
import Domain.Spies.Spy ( Spy )

data CanDrive    = Car | Boat | Helicopter   deriving Show
data CanDo       = AquaticMission | AeroMission deriving Show
data CanPayment  = Cash         deriving Show
data Specs a = Drive CanDrive | Do CanDo | Payment CanPayment deriving Show

type SpecifiedSpy :: [Specs a] -> Type
data SpecifiedSpy specifications where SpecifiedSpy :: Spy -> SpecifiedSpy specifications deriving Show

specifiedSpy :: Spy -> SpecifiedSpy '[]
specifiedSpy = SpecifiedSpy

specifiedSpies :: [Spy] -> [SpecifiedSpy '[]]
specifiedSpies = foldr (\ spy -> (<>) [specifiedSpy spy]) []