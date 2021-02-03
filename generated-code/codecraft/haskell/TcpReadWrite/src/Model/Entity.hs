module Model.Entity where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Model.EntityType (EntityType)
import Model.Vec2Int32 (Vec2Int32)

-- | Game entity
data Entity = Entity {
    -- | Entity's ID. Unique for each entity
    id :: Int32,
    -- | Entity's owner player ID, if owned by a player
    playerId :: Maybe Int32,
    -- | Entity's type
    entityType :: EntityType,
    -- | Entity's position (corner with minimal coordinates)
    position :: Vec2Int32,
    -- | Current health
    health :: Int32,
    -- | If entity is active, it can perform actions
    active :: Bool }
    deriving Show

instance Trans Entity where
    read = do
        id <- Trans.read
        playerId <- Trans.read
        entityType <- Trans.read
        position <- Trans.read
        health <- Trans.read
        active <- Trans.read
        return Entity {
            id,
            playerId,
            entityType,
            position,
            health,
            active }

    write Entity {
        id,
        playerId,
        entityType,
        position,
        health,
        active } = do
            Trans.write id
            Trans.write playerId
            Trans.write entityType
            Trans.write position
            Trans.write health
            Trans.write active