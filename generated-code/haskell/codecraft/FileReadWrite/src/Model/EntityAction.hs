module Model.EntityAction where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Model.AttackAction (AttackAction)
import Model.BuildAction (BuildAction)
import Model.MoveAction (MoveAction)
import Model.RepairAction (RepairAction)

-- | Entity's action
data EntityAction = EntityAction {
    -- | Move action
    moveAction :: Maybe MoveAction,
    -- | Build action
    buildAction :: Maybe BuildAction,
    -- | Attack action
    attackAction :: Maybe AttackAction,
    -- | Repair action
    repairAction :: Maybe RepairAction }
    deriving Show

instance Trans EntityAction where
    read = do
        moveAction <- Trans.read
        buildAction <- Trans.read
        attackAction <- Trans.read
        repairAction <- Trans.read
        return EntityAction {
            moveAction,
            buildAction,
            attackAction,
            repairAction }
    
    write EntityAction {
        moveAction,
        buildAction,
        attackAction,
        repairAction } = do
            Trans.write moveAction
            Trans.write buildAction
            Trans.write attackAction
            Trans.write repairAction