module Model.Structure where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)


data Structure = Structure {
    text :: String,
    floatNumber :: Float,
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