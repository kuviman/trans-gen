module Model.BuildAction where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Model.EntityType (EntityType)
import Vec2Int32 (Vec2Int32)

-- | Build action
data BuildAction = BuildAction {
    -- | Type of an entity to build
    entityType :: EntityType,
    -- | Desired position of new entity
    position :: Vec2Int32 }
    deriving Show

instance Trans BuildAction where
    read = do
        entityType <- Trans.read
        position <- Trans.read
        return BuildAction {
            entityType,
            position }

    write BuildAction {
        entityType,
        position } = do
            Trans.write entityType
            Trans.write position