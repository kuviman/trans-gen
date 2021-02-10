module Model.EntityProperties where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Model.AttackProperties (AttackProperties)
import Model.BuildProperties (BuildProperties)
import Model.RepairProperties (RepairProperties)

-- | Entity properties
data EntityProperties = EntityProperties {
    -- | Size. Entity has a form of a square with side of this length
    size :: Int32,
    -- | Score for building this entity
    buildScore :: Int32,
    -- | Score for destroying this entity
    destroyScore :: Int32,
    -- | Whether this entity can move
    canMove :: Bool,
    -- | Number of population points this entity provides, if active
    populationProvide :: Int32,
    -- | Number of population points this entity uses
    populationUse :: Int32,
    -- | Maximum health points
    maxHealth :: Int32,
    -- | Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
    initialCost :: Int32,
    -- | If fog of war is enabled, maximum distance at which other entities are considered visible
    sightRange :: Int32,
    -- | Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
    resourcePerHealth :: Int32,
    -- | Build properties, if entity can build
    build :: Maybe BuildProperties,
    -- | Attack properties, if entity can attack
    attack :: Maybe AttackProperties,
    -- | Repair properties, if entity can repair
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