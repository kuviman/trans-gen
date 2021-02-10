module Vec2Int32 where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int

-- | 2 dimensional vector.
data Vec2Int32 = Vec2Int32 {
    -- | `x` coordinate of the vector
    x :: Int32,
    -- | `y` coordinate of the vector
    y :: Int32 }
    deriving Show

instance Trans Vec2Int32 where
    read = do
        x <- Trans.read
        y <- Trans.read
        return Vec2Int32 {
            x,
            y }

    write Vec2Int32 {
        x,
        y } = do
            Trans.write x
            Trans.write y