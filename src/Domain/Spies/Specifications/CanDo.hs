{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Domain.Spies.Specifications.CanDo where
import Domain.Spies.Specifications.SpySpecification (SpecifiedSpy (SpecifiedSpy), Specs(..), CanDrive(..), CanDo(..))
import Common.TypeOperators (Satisfied, And, Expression(Spec))
import Domain.Spies.Spy (age)

canDoWater :: Satisfied (And (Spec (Drive Car)) (Spec (Drive Boat))) specs => SpecifiedSpy specs -- ^ 
  -> Maybe (SpecifiedSpy (Do WaterMission ': specs))
canDoWater (SpecifiedSpy spy) = 
    if age spy >= 35 then Just $ SpecifiedSpy spy
    else Nothing