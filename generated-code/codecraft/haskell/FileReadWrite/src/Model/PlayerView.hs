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

-- | Information available to the player
data PlayerView = PlayerView {
    -- | Your player's ID
    myId :: Int32,
    -- | Size of the map
    mapSize :: Int32,
    -- | Whether fog of war is enabled
    fogOfWar :: Bool,
    -- | Entity properties for each entity type
    entityProperties :: Map EntityType EntityProperties,
    -- | Max tick count for the game
    maxTickCount :: Int32,
    -- | Max pathfind nodes when performing pathfinding in the game simulator
    maxPathfindNodes :: Int32,
    -- | Current tick
    currentTick :: Int32,
    -- | List of players
    players :: [Player],
    -- | List of entities
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