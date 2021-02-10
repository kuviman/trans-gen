module Model.AttackAction where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Model.AutoAttack (AutoAttack)

-- | Attack action
data AttackAction = AttackAction {
    -- | If specified, target entity's ID
    target :: Maybe Int32,
    -- | If specified, configures auto attacking
    autoAttack :: Maybe AutoAttack }
    deriving Show

instance Trans AttackAction where
    read = do
        target <- Trans.read
        autoAttack <- Trans.read
        return AttackAction {
            target,
            autoAttack }
    
    write AttackAction {
        target,
        autoAttack } = do
            Trans.write target
            Trans.write autoAttack