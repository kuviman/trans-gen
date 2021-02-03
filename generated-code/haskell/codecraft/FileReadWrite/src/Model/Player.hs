module Model.Player where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int

-- | Player (strategy, client)
data Player = Player {
    -- | Player's ID
    id :: Int32,
    -- | Current score
    score :: Int32,
    -- | Current amount of resource
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