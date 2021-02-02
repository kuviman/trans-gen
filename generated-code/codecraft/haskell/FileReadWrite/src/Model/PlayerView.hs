module Model.PlayerView where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Data.Map
import Model.Entity (Entity)
import Model.EntityProperties (EntityProperties)
import Model.EntityType (EntityType)
import Model.Player (Player)

data PlayerView = PlayerView {
    myId :: Int32,
    mapSize :: Int32,
    fogOfWar :: Bool,
    entityProperties :: Map EntityType EntityProperties,
    maxTickCount :: Int32,
    maxPathfindNodes :: Int32,
    currentTick :: Int32,
    players :: [Player],
    entities :: [Entity] }
    deriving Show

instance Trans PlayerView where
    read = do
        myId <- Trans.read
        mapSize <- Trans.read
        fogOfWar <- Trans.read
        entityProperties <- Trans.read
        maxTickCount <- Trans.read
        maxPathfindNodes <- Trans.read
        currentTick <- Trans.read
        players <- Trans.read
        entities <- Trans.read
        return PlayerView {
            myId,
            mapSize,
            fogOfWar,
            entityProperties,
            maxTickCount,
            maxPathfindNodes,
            currentTick,
            players,
            entities }

    write PlayerView {
        myId,
        mapSize,
        fogOfWar,
        entityProperties,
        maxTickCount,
        maxPathfindNodes,
        currentTick,
        players,
        entities } = do
            Trans.write myId
            Trans.write mapSize
            Trans.write fogOfWar
            Trans.write entityProperties
            Trans.write maxTickCount
            Trans.write maxPathfindNodes
            Trans.write currentTick
            Trans.write players
            Trans.write entities