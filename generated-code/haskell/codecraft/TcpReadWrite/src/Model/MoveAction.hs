module Model.MoveAction where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Vec2Int32 (Vec2Int32)

-- | Move action
data MoveAction = MoveAction {
    -- | Target position
    target :: Vec2Int32,
    -- | Whether to try find closest position, if path to target is not found
    findClosestPosition :: Bool,
    -- | Whether to destroy other entities on the way
    breakThrough :: Bool }
    deriving Show

instance Trans MoveAction where
    read = do
        target <- Trans.read
        findClosestPosition <- Trans.read
        breakThrough <- Trans.read
        return MoveAction {
            target,
            findClosestPosition,
            breakThrough }

    write MoveAction {
        target,
        findClosestPosition,
        breakThrough } = do
            Trans.write target
            Trans.write findClosestPosition
            Trans.write breakThrough