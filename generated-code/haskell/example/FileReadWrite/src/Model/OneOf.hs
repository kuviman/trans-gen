module Model.OneOf where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int

-- | First option
data OneOfOptionOne = OneOfOptionOne {
    -- | List of integers
    vecInt32 :: [Int32],
    -- | Long integer
    longInt :: Int64 }
    deriving Show

instance Trans OneOfOptionOne where
    read = do
        vecInt32 <- Trans.read
        longInt <- Trans.read
        return OneOfOptionOne {
            vecInt32,
            longInt }

    write OneOfOptionOne {
        vecInt32,
        longInt } = do
            Trans.write vecInt32
            Trans.write longInt

-- | Second option
data OneOfOptionTwo = OneOfOptionTwo {
    -- | usize
    value :: Int32 }
    deriving Show

instance Trans OneOfOptionTwo where
    read = do
        value <- Trans.read
        return OneOfOptionTwo {
            value }

    write OneOfOptionTwo {
        value } = do
            Trans.write value

-- | Oneof example
data OneOf
    -- | First option
    = OptionOne OneOfOptionOne
    -- | Second option
    | OptionTwo OneOfOptionTwo
    deriving Show

instance Trans OneOf where
    read = do
        tag :: Int32 <- Trans.read
        case tag of
            0 -> OptionOne <$> Trans.read
            1 -> OptionTwo <$> Trans.read

    write (OptionOne value) = do
        Trans.write (0 :: Int32)
        Trans.write value
    write (OptionTwo value) = do
        Trans.write (1 :: Int32)
        Trans.write value