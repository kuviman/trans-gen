module Model.DebugInterface.PrimitiveType where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int

-- | Primitive type for debug rendering
data PrimitiveType
    -- | Lines, number of vertices should be divisible by 2
    = Lines
    -- | Triangles, number of vertices should be divisible by 3
    | Triangles
    deriving (Eq, Ord, Show)

instance Trans PrimitiveType where
    read = do
        tag :: Int32 <- Trans.read
        return $ case tag of
            0 -> Lines
            1 -> Triangles

    write Lines =
        Trans.write (0 :: Int32)
    write Triangles =
        Trans.write (1 :: Int32)