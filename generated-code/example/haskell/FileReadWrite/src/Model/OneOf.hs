module Model.OneOf where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int

data OneOfOptionOne = OneOfOptionOne {
    vecInt32 :: [Int32],
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

data OneOfOptionTwo = OneOfOptionTwo {
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

data OneOf
    = OptionOne OneOfOptionOne
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