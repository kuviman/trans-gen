module Model.Action where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Data.Map
import Model.EntityAction (EntityAction)

-- | Player's action
data Action = Action {
    -- | New actions for entities. If entity does not get new action, if will continue to perform previously set one
    entityActions :: Map Int32 EntityAction }
    deriving Show

instance Trans Action where
    read = do
        entityActions <- Trans.read
        return Action {
            entityActions }
    
    write Action {
        entityActions } = do
            Trans.write entityActions