module Model.AutoAttack where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Model.EntityType (EntityType)

-- | Auto attack options
data AutoAttack = AutoAttack {
    -- | Maximum distance to pathfind
    pathfindRange :: Int32,
    -- | List of target entity types to try to attack. If empty, all types but resource are considered
    validTargets :: [EntityType] }
    deriving Show

instance Trans AutoAttack where
    read = do
        pathfindRange <- Trans.read
        validTargets <- Trans.read
        return AutoAttack {
            pathfindRange,
            validTargets }

    write AutoAttack {
        pathfindRange,
        validTargets } = do
            Trans.write pathfindRange
            Trans.write validTargets