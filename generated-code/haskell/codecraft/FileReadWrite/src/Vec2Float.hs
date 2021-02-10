module Vec2Float where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)


-- | 2 dimensional vector.
data Vec2Float = Vec2Float {
    -- | `x` coordinate of the vector
    x :: Float,
    -- | `y` coordinate of the vector
    y :: Float }
    deriving Show

instance Trans Vec2Float where
    read = do
        x <- Trans.read
        y <- Trans.read
        return Vec2Float {
            x,
            y }

    write Vec2Float {
        x,
        y } = do
            Trans.write x
            Trans.write y