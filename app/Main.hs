{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Common.Operators (eval, type (#<>), Expression (Spec), Satisfied, Elem)
import Domain.Spies.Specifications (
    SpecifiedSpy, specifiedSpies, specifiedSpy,
    CanDo (AquaticMission), CanDrive(..), Specs(Drive, Do),
    canDriveCar, canPilotBoat,
    canDoAeroMission, canDoAquaticMission)
import Domain.Spies.Spy (Spy (..))
import Domain.Missions.Mission (mkAquaticMission, mkInvalidMission, mkAeroMission, execute, complete, transition, Mission, State(..), MissionInfo)
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
aquaticMission :: SpecifiedSpy [ 'Drive 'Boat, 'Drive 'Car] -> Double -> Mission (MissionInfo Spy Double) 'Pending
aquaticMission spy value = case canDoAquaticMission spy of
    Just specified ->  mkAquaticMission specified value
    Nothing -> mkInvalidMission

-- Montanto uma lista de Aquatic mission e tendo certeza que todos os itens sao de missoes aquaticas e com status pendente
-- Achei bacana poder usar GADTs na construcao dessa specification
aquaticMissions :: [Mission (MissionInfo spy value) state] -> [SpecifiedMission '[ 'IsAquatic] 'Pending]
aquaticMissions = mapMaybe ((isPending Control.Monad.>=> isAquatic) . specifiedMission)

-- Beleza testamos o booleano (&&) que era igual a implementacao do validador de strings, vamos para casos mais interessantes
{-- 
    Agora temos a regra de negocio que diz se um espiao pode fazer uma missao aerea, basicamente nossa regra diz:
    Ou ele pilota um helicoptero ou ele nao dirige um carro
    portanto seriam specs validas todas as listas que contem a especificacao de pilotar helicoptero e as listas que não 
    contem a spec de dirigir o carro.
--}

aeroMissionForSpyWhoCanPilotHelicopter :: SpecifiedSpy '[Drive Helicopter] -> Double -> Mission (MissionInfo Spy Double) 'Pending
aeroMissionForSpyWhoCanPilotHelicopter spy value = case canDoAeroMission spy of
    Just specified ->  mkAeroMission specified value
    Nothing -> mkInvalidMission

{--
    Abaixo temos o comportamento ruim do nosso operador Not, basta que nossa lista não contenha (Drive Car) mas para não conter
    basta não testar o espião com a função canDriveCar, deveriamos nos preocupar em gerar specs negando permissoes e forçar que
    na lista de spes tenha algo do tipo (NotDrive Car) assim teriamos o comportamento esperado.
--}

aeroMissionForSpyWhoCanPilotBoat :: SpecifiedSpy '[Drive Boat] -> Double -> Mission (MissionInfo Spy Double) 'Pending
aeroMissionForSpyWhoCanPilotBoat spy value = case canDoAeroMission spy of
    Just specified ->  mkAeroMission specified value
    Nothing -> mkInvalidMission

{--
    Exemplo de um código que não compila pois temos a spec Drive Car na lista de specs

    aeroMissionForSpyWhoCanPilotCar :: SpecifiedSpy '[Drive Car] -> Double -> Mission (MissionInfo Spy Double) 'Pending
    aeroMissionForSpyWhoCanPilotCar spy value = case canDoAeroMission spy of
        Just specified ->  mkAeroMission specified value
        Nothing -> mkInvalidMission
--}


aquaticMissionsToString :: [String]
aquaticMissionsToString = map
    ((\aquaticMission -> show aquaticMission ++ "\n") . (`aquaticMission` 1200)) 
    (spiesWithAuthForPilotBoatAndDriveCar spies)

main :: IO ()
main = putStr $ unlines aquaticMissionsToString