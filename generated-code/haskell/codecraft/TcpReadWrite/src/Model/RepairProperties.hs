module Model.RepairProperties where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Model.EntityType (EntityType)

-- | Entity's repair properties
data RepairProperties = RepairProperties {
    -- | Valid target entity types
    validTargets :: [EntityType],
    -- | Health restored in one tick
    power :: Int32 }
    deriving Show

instance Trans RepairProperties where
    read = do
        validTargets <- Trans.read
        power <- Trans.read
        return RepairProperties {
            validTargets,
            power }

    write RepairProperties {
        validTargets,
        power } = do
            Trans.write validTargets
            Trans.write power