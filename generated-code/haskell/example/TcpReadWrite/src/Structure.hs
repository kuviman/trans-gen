module Structure where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)


-- | Example structure
data Structure = Structure {
    -- | Text
    text :: String,
    -- | 32-bit float
    floatNumber :: Float,
    -- | 64-bit float
    doubleNumber :: Double }
    deriving Show

instance Trans Structure where
    read = do
        text <- Trans.read
        floatNumber <- Trans.read
        doubleNumber <- Trans.read
        return Structure {
            text,
            floatNumber,
            doubleNumber }

    write Structure {
        text,
        floatNumber,
        doubleNumber } = do
            Trans.write text
            Trans.write floatNumber
            Trans.write doubleNumber