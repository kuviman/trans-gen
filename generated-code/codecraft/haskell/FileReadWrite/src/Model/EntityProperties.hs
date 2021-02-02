module Model.EntityProperties where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Model.AttackProperties (AttackProperties)
import Model.BuildProperties (BuildProperties)
import Model.RepairProperties (RepairProperties)

data EntityProperties = EntityProperties {
    size :: Int32,
    buildScore :: Int32,
    destroyScore :: Int32,
    canMove :: Bool,
    populationProvide :: Int32,
    populationUse :: Int32,
    maxHealth :: Int32,
    initialCost :: Int32,
    sightRange :: Int32,
    resourcePerHealth :: Int32,
    build :: Maybe BuildProperties,
    attack :: Maybe AttackProperties,
    repair :: Maybe RepairProperties }
    deriving Show

instance Trans EntityProperties where
    read = do
        size <- Trans.read
        buildScore <- Trans.read
        destroyScore <- Trans.read
        canMove <- Trans.read
        populationProvide <- Trans.read
        populationUse <- Trans.read
        maxHealth <- Trans.read
        initialCost <- Trans.read
        sightRange <- Trans.read
        resourcePerHealth <- Trans.read
        build <- Trans.read
        attack <- Trans.read
        repair <- Trans.read
        return EntityProperties {
            size,
            buildScore,
            destroyScore,
            canMove,
            populationProvide,
            populationUse,
            maxHealth,
            initialCost,
            sightRange,
            resourcePerHealth,
            build,
            attack,
            repair }

    write EntityProperties {
        size,
        buildScore,
        destroyScore,
        canMove,
        populationProvide,
        populationUse,
        maxHealth,
        initialCost,
        sightRange,
        resourcePerHealth,
        build,
        attack,
        repair } = do
            Trans.write size
            Trans.write buildScore
            Trans.write destroyScore
            Trans.write canMove
            Trans.write populationProvide
            Trans.write populationUse
            Trans.write maxHealth
            Trans.write initialCost
            Trans.write sightRange
            Trans.write resourcePerHealth
            Trans.write build
            Trans.write attack
            Trans.write repair