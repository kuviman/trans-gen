module Codegame.ServerMessage where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Model.PlayerView (PlayerView)

-- | Get action for next tick
data ServerMessageGetAction = ServerMessageGetAction {
    -- | Player's view
    playerView :: PlayerView,
    -- | Whether app is running with debug interface available
    debugAvailable :: Bool }
    deriving Show

instance Trans ServerMessageGetAction where
    read = do
        playerView <- Trans.read
        debugAvailable <- Trans.read
        return ServerMessageGetAction {
            playerView,
            debugAvailable }
    
    write ServerMessageGetAction {
        playerView,
        debugAvailable } = do
            Trans.write playerView
            Trans.write debugAvailable

-- | Signifies end of the game
data ServerMessageFinish = ServerMessageFinish { }
    deriving Show

instance Trans ServerMessageFinish where
    read = do
        return ServerMessageFinish { }
    
    write ServerMessageFinish { } = do
            return ()

-- | Debug update
data ServerMessageDebugUpdate = ServerMessageDebugUpdate {
    -- | Player's view
    playerView :: PlayerView }
    deriving Show

instance Trans ServerMessageDebugUpdate where
    read = do
        playerView <- Trans.read
        return ServerMessageDebugUpdate {
            playerView }
    
    write ServerMessageDebugUpdate {
        playerView } = do
            Trans.write playerView

-- | Message sent from server
data ServerMessage
    -- | Get action for next tick
    = GetAction ServerMessageGetAction
    -- | Signifies end of the game
    | Finish ServerMessageFinish
    -- | Debug update
    | DebugUpdate ServerMessageDebugUpdate
    deriving Show

instance Trans ServerMessage where
    read = do
        tag :: Int32 <- Trans.read
        case tag of
            0 -> GetAction <$> Trans.read
            1 -> Finish <$> Trans.read
            2 -> DebugUpdate <$> Trans.read
    
    write (GetAction value) = do
        Trans.write (0 :: Int32)
        Trans.write value
    write (Finish value) = do
        Trans.write (1 :: Int32)
        Trans.write value
    write (DebugUpdate value) = do
        Trans.write (2 :: Int32)
        Trans.write value