module Main where

import Spies (Spy(..), canDoWaterMissions, specifiedSpies)
import Repository (find)

spies :: [Spy]
spies = [Spy "Marcos" 23, Spy "Marcos" 21, Spy "Jorge" 18]

main :: IO ()
main = print (find canDoWaterMissions (specifiedSpies spies))
