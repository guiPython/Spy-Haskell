{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Common.Operators (eval, type (#<>), Expression (Spec), Satisfied, Elem)
import Domain.Spies.Specifications.CanDo (canDoAquaticMission)
import Domain.Spies.Specifications.CanPilot (canDriveCar, canPilotBoat)
import Domain.Spies.Specifications.SpySpecification (SpecifiedSpy (SpecifiedSpy), specifiedSpies, specifiedSpy, CanDo (AquaticMission), CanDrive(..), Specs(Drive, Do))
import Domain.Spies.Spy (Spy (..))
import Domain.Missions.Mission (mkAquaticMission, mkInvalidMission, execute, complete, transition, Mission, State(..), MissionInfo)
import Domain.Missions.Specifications (isAquatic, specifiedMission, isAero, isPending, SpecifiedMission(..), Specs(..))
import Data.Maybe (mapMaybe)
import qualified Control.Monad

-- Lista de espiões sem nenhuma especificação
spies :: [Spy]
spies = [Spy "George" 15, Spy "Thomas" 18, Spy "Mohamed" 10, Spy "C-137" 59]

-- Lista de espiões que podem pilotar um barco
spiesWithAuthForPilotBoat :: [Spy] -> [SpecifiedSpy '[ 'Drive 'Boat]]
spiesWithAuthForPilotBoat = mapMaybe (canPilotBoat . specifiedSpy)

-- Lista de espioes que podem pilotar um barco e um carro
spiesWithAuthForPilotBoatAndDriveCar :: [Spy] -> [SpecifiedSpy [ 'Drive 'Boat, 'Drive 'Car]]
spiesWithAuthForPilotBoatAndDriveCar = mapMaybe ((canDriveCar Control.Monad.>=> canPilotBoat) . specifiedSpy)

-- Lista de espioes que podem fazer missoes aquaticas 
-- Espioes que podem fazer missoes aquaticas sao aquele que dirigem carro (&&) pilotam barco
-- Sendo assim a lista spiesWithAuthForPilotBoatAndDriveCar com k spioes deve gerar k missoes aquaticas

-- Vamos construir uma missao aquatica do jeito errado
{--
    aquaticMission :: Mission * Pending
    aquaticMission = mkAquaticMission (head spiesWithAuthForPilotBoatAndDriveCar spies) 15.50

    Voce pode estar se perguntando o porque o compilador nos diz isso, simples ele nao pode inferir que
    por saber dirigir um carro e pilotar um barco o espiao pode fazer uma missao aquatica, por isso fazemos o 
    haskell dar essa especificacao ao espiao com a funcao canDoAquaticMission, para melhorar fazemos o compilador
    provar que todo espiao que entrar nessa funcao tenha essas duas especificacoes anteriormente, ou seja, ao 
    receber a especificacao ['Do 'AquaticMission] temos certeza que ele consegue dirigir um carro e pilotar um barco
--}

-- Agora o jeito correto
aquaticMission :: SpecifiedSpy [ 'Drive 'Boat, 'Drive 'Car] -> Mission (MissionInfo Spy Double) 'Pending
aquaticMission spy = case canDoAquaticMission spy of
    Just specified ->  mkAquaticMission specified 15.50
    Nothing -> mkInvalidMission

-- Montanto uma lista de Aquatic mission e tendo certeza que todos os itens sao de missoes aquaticas e com status pendente
-- Achei bacana poder usar GADTs na construcao dessa specification
aquaticMissions :: [Mission (MissionInfo spy value) state] -> [SpecifiedMission '[ 'IsAquatic] 'Pending]
aquaticMissions = mapMaybe ((isPending Control.Monad.>=> isAquatic) . specifiedMission)

-- Beleza testamos o booleano (&&) que era igual a implementacao do validador de strings, vamos para casos mais interessantes
{-- 
    Agora temos a regra de negocio que diz se um espiao pode fazer uma missao aerea, basicamente nossa regra diz:
    Ou ele pilota um helicoptero ou ele nao dirige um carro
    portanto seriam specs validas todas as lista que contem a especificacao de pilotar helicoptero e outro conjunto de specs que 
    nao contem permissao de dirigir um Carro 
--}

main :: IO ()
main = print $ show (head $ spiesWithAuthForPilotBoatAndDriveCar spies)