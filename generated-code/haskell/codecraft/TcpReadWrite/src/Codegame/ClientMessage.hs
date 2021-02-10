module Codegame.ClientMessage where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Codegame.DebugCommand (DebugCommand)
import Data.Int
import Model.Action (Action)

-- | Ask app to perform new debug command
data ClientMessageDebugMessage = ClientMessageDebugMessage {
    -- | Command to perform
    command :: DebugCommand }
    deriving Show

instance Trans ClientMessageDebugMessage where
    read = do
        command <- Trans.read
        return ClientMessageDebugMessage {
            command }

    write ClientMessageDebugMessage {
        command } = do
            Trans.write command

-- | Reply for ServerMessage::GetAction
data ClientMessageActionMessage = ClientMessageActionMessage {
    -- | Player's action
    action :: Action }
    deriving Show

instance Trans ClientMessageActionMessage where
    read = do
        action <- Trans.read
        return ClientMessageActionMessage {
            action }

    write ClientMessageActionMessage {
        action } = do
            Trans.write action

-- | Signifies finish of the debug update
data ClientMessageDebugUpdateDone = ClientMessageDebugUpdateDone { }
    deriving Show

instance Trans ClientMessageDebugUpdateDone where
    read = do
        return ClientMessageDebugUpdateDone { }

    write ClientMessageDebugUpdateDone { } = do
            return ()

-- | Request debug state from the app
data ClientMessageRequestDebugState = ClientMessageRequestDebugState { }
    deriving Show

instance Trans ClientMessageRequestDebugState where
    read = do
        return ClientMessageRequestDebugState { }

    write ClientMessageRequestDebugState { } = do
            return ()

-- | Message sent from client
data ClientMessage
    -- | Ask app to perform new debug command
    = DebugMessage ClientMessageDebugMessage
    -- | Reply for ServerMessage::GetAction
    | ActionMessage ClientMessageActionMessage
    -- | Signifies finish of the debug update
    | DebugUpdateDone ClientMessageDebugUpdateDone
    -- | Request debug state from the app
    | RequestDebugState ClientMessageRequestDebugState
    deriving Show

instance Trans ClientMessage where
    read = do
        tag :: Int32 <- Trans.read
        case tag of
            0 -> DebugMessage <$> Trans.read
            1 -> ActionMessage <$> Trans.read
            2 -> DebugUpdateDone <$> Trans.read
            3 -> RequestDebugState <$> Trans.read

    write (DebugMessage value) = do
        Trans.write (0 :: Int32)
        Trans.write value
    write (ActionMessage value) = do
        Trans.write (1 :: Int32)
        Trans.write value
    write (DebugUpdateDone value) = do
        Trans.write (2 :: Int32)
        Trans.write value
    write (RequestDebugState value) = do
        Trans.write (3 :: Int32)
        Trans.write value