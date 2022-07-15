{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Domain.Spies.Specifications.CanPilot where
import Domain.Spies.Specifications.SpySpecification
import Domain.Spies.Spy ( Spy(age) )

canPilotBoat :: SpecifiedSpy specifications -> Maybe(SpecifiedSpy (Drive Boat ':specifications))
canPilotBoat (SpecifiedSpy spy) =
    if age spy >= 18 then Just $ SpecifiedSpy spy
    else Nothing

canDriveCar :: SpecifiedSpy specifications -> Maybe(SpecifiedSpy (Drive Car ': specifications))
canDriveCar (SpecifiedSpy spy) =
    if age spy >= 18 then Just $ SpecifiedSpy spy
    else Nothing