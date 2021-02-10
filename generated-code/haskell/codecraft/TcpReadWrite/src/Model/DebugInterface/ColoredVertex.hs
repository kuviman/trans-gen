module Model.DebugInterface.ColoredVertex where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Color (Color)
import Vec2Float (Vec2Float)

-- | Vertex for debug rendering
data ColoredVertex = ColoredVertex {
    -- | Position in world coordinates (if none, screen position (0, 0) is used)
    worldPos :: Maybe Vec2Float,
    -- | Additional offset in screen coordinates
    screenOffset :: Vec2Float,
    -- | Color to use
    color :: Color }
    deriving Show

instance Trans ColoredVertex where
    read = do
        worldPos <- Trans.read
        screenOffset <- Trans.read
        color <- Trans.read
        return ColoredVertex {
            worldPos,
            screenOffset,
            color }
    
    write ColoredVertex {
        worldPos,
        screenOffset,
        color } = do
            Trans.write worldPos
            Trans.write screenOffset
            Trans.write color