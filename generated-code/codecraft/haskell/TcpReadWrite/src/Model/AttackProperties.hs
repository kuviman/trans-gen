module Model.AttackProperties where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int

data AttackProperties = AttackProperties {
    attackRange :: Int32,
    damage :: Int32,
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