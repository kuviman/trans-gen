module Model.RepairProperties where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Model.EntityType (EntityType)

data RepairProperties = RepairProperties {
    validTargets :: [EntityType],
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