module Model.RepairAction where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int

-- | Repair action
data RepairAction = RepairAction {
    -- | Target entity's ID
    target :: Int32 }
    deriving Show

instance Trans RepairAction where
    read = do
        target <- Trans.read
        return RepairAction {
            target }

    write RepairAction {
        target } = do
            Trans.write target