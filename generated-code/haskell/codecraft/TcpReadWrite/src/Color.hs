module Color where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)


-- | RGBA Color
data Color = Color {
    -- | Red component
    r :: Float,
    -- | Green component
    g :: Float,
    -- | Blue component
    b :: Float,
    -- | Alpha (opacity) component
    a :: Float }
    deriving Show

instance Trans Color where
    read = do
        r <- Trans.read
        g <- Trans.read
        b <- Trans.read
        a <- Trans.read
        return Color {
            r,
            g,
            b,
            a }

    write Color {
        r,
        g,
        b,
        a } = do
            Trans.write r
            Trans.write g
            Trans.write b
            Trans.write a