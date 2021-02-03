module Model.AttackProperties where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int

-- | Entity's attack properties
data AttackProperties = AttackProperties {
    -- | Maximum attack range
    attackRange :: Int32,
    -- | Damage dealt in one tick
    damage :: Int32,
    -- | If true, dealing damage will collect resource from target
    collectResource :: Bool }
    deriving Show

instance Trans AttackProperties where
    read = do
        attackRange <- Trans.read
        damage <- Trans.read
        collectResource <- Trans.read
        return AttackProperties {
            attackRange,
            damage,
            collectResource }

    write AttackProperties {
        attackRange,
        damage,
        collectResource } = do
            Trans.write attackRange
            Trans.write damage
            Trans.write collectResource