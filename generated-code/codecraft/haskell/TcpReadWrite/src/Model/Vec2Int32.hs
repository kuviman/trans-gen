module Model.Vec2Int32 where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int

data Vec2Int32 = Vec2Int32 {
    x :: Int32,
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