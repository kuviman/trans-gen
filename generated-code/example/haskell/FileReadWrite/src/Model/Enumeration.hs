module Model.Enumeration where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int

-- | Example enumeration
data Enumeration
    -- | First option
    = ValueOne
    -- | Second option
    | ValueTwo
    deriving (Eq, Ord, Show)

instance Trans Enumeration where
    read = do
        tag :: Int32 <- Trans.read
        return $ case tag of
            0 -> ValueOne
            1 -> ValueTwo

    write ValueOne =
        Trans.write (0 :: Int32)
    write ValueTwo =
        Trans.write (1 :: Int32)