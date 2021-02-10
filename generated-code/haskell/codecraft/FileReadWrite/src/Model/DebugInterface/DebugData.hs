module Model.DebugInterface.DebugData where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Model.DebugInterface.ColoredVertex (ColoredVertex)
import Model.DebugInterface.PrimitiveType (PrimitiveType)

-- | Log some text
data DebugDataLog = DebugDataLog {
    -- | Text to show
    text :: String }
    deriving Show

instance Trans DebugDataLog where
    read = do
        text <- Trans.read
        return DebugDataLog {
            text }
    
    write DebugDataLog {
        text } = do
            Trans.write text

-- | Draw primitives
data DebugDataPrimitives = DebugDataPrimitives {
    -- | Vertices
    vertices :: [ColoredVertex],
    -- | Primitive type
    primitiveType :: PrimitiveType }
    deriving Show

instance Trans DebugDataPrimitives where
    read = do
        vertices <- Trans.read
        primitiveType <- Trans.read
        return DebugDataPrimitives {
            vertices,
            primitiveType }
    
    write DebugDataPrimitives {
        vertices,
        primitiveType } = do
            Trans.write vertices
            Trans.write primitiveType

-- | Draw text
data DebugDataPlacedText = DebugDataPlacedText {
    -- | Vertex to determine text position and color
    vertex :: ColoredVertex,
    -- | Text
    text :: String,
    -- | Text alignment (0 means left, 0.5 means center, 1 means right)
    alignment :: Float,
    -- | Font size in pixels
    size :: Float }
    deriving Show

instance Trans DebugDataPlacedText where
    read = do
        vertex <- Trans.read
        text <- Trans.read
        alignment <- Trans.read
        size <- Trans.read
        return DebugDataPlacedText {
            vertex,
            text,
            alignment,
            size }
    
    write DebugDataPlacedText {
        vertex,
        text,
        alignment,
        size } = do
            Trans.write vertex
            Trans.write text
            Trans.write alignment
            Trans.write size

-- | Debug data can be drawn in the app
data DebugData
    -- | Log some text
    = Log DebugDataLog
    -- | Draw primitives
    | Primitives DebugDataPrimitives
    -- | Draw text
    | PlacedText DebugDataPlacedText
    deriving Show

instance Trans DebugData where
    read = do
        tag :: Int32 <- Trans.read
        case tag of
            0 -> Log <$> Trans.read
            1 -> Primitives <$> Trans.read
            2 -> PlacedText <$> Trans.read
    
    write (Log value) = do
        Trans.write (0 :: Int32)
        Trans.write value
    write (Primitives value) = do
        Trans.write (1 :: Int32)
        Trans.write value
    write (PlacedText value) = do
        Trans.write (2 :: Int32)
        Trans.write value