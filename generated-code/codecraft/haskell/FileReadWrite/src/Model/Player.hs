module Model.Player where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int

data Player = Player {
    id :: Int32,
    score :: Int32,
    resource :: Int32 }
    deriving Show

instance Trans Player where
    read = do
        id <- Trans.read
        score <- Trans.read
        resource <- Trans.read
        return Player {
            id,
            score,
            resource }

    write Player {
        id,
        score,
        resource } = do
            Trans.write id
            Trans.write score
            Trans.write resource