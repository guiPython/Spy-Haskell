{-# LANGUAGE PolyKinds #-}

module Main where

import Common.TypeOperators (eval)
import Domain.Spies.Specifications.CanDo (canDoWater)
import Domain.Spies.Specifications.CanPilot (canDriveCar, canPilotBoat)
import Domain.Spies.Specifications.SpySpecification (SpecifiedSpy, specifiedSpies, specifiedSpy)
import Domain.Spies.Spy (Spy (..))

spy = Spy "C-137" 58

spies = [Spy "George" 15, Spy "Thomas" 18, Spy "Mohamed" 10]

valid specified = canDriveCar specified >>= canPilotBoat >>= canDoWater

main :: IO ()
main =
  print $
    name spy
      ++ if eval $ valid $ specifiedSpy spy
        then " esta apto para missoes em agua"
        else "nao esta apto para missoes em agua"