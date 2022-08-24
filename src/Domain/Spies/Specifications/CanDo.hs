{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Domain.Spies.Specifications.CanDo where
import Domain.Spies.Specifications.SpySpecification (SpecifiedSpy (SpecifiedSpy), Specs(..), CanDrive(..), CanDo(..))
import Common.Operators (Satisfied, And, Expression(Spec), Or, NoT)
import Domain.Spies.Spy (age)

canDoAquaticMission :: Satisfied (And (Spec (Drive Car)) (Spec (Drive Boat))) specs => SpecifiedSpy specs -- ^ 
  -> Maybe (SpecifiedSpy (Do AquaticMission ': specs))
canDoAquaticMission (SpecifiedSpy spy) = 
    if age spy >= 35 then Just $ SpecifiedSpy spy
    else Nothing

canDoAeroMission :: Satisfied (Or (Spec (Drive Helicopter)) (Spec (Drive Car))) specs => SpecifiedSpy  specs -- ^ 
  -> Maybe(SpecifiedSpy (Do AeroMission ': specs))
canDoAeroMission (SpecifiedSpy spy) = 
    if age spy >= 35 then Just $ SpecifiedSpy spy
    else Nothing