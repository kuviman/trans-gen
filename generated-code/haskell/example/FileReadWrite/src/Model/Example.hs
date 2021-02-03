module Model.Example where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Data.Map
import Model.Enumeration (Enumeration)
import Model.OneOf (OneOf)
import Model.Structure (Structure)

-- | Example
data Example = Example {
    -- | OneOf
    oneOf :: OneOf,
    -- | Dictionary
    hashMap :: Map Enumeration Int32,
    -- | Optional int
    optionalInt :: Maybe Int32,
    -- | Optional boolean
    optionalBool :: Maybe Bool,
    -- | Optional OneOf
    optionalOneOf :: Maybe OneOf,
    -- | Optional struct
    optionalStruct :: Maybe Structure,
    -- | Optional enum
    optionalEnum :: Maybe Enumeration }
    deriving Show

instance Trans Example where
    read = do
        oneOf <- Trans.read
        hashMap <- Trans.read
        optionalInt <- Trans.read
        optionalBool <- Trans.read
        optionalOneOf <- Trans.read
        optionalStruct <- Trans.read
        optionalEnum <- Trans.read
        return Example {
            oneOf,
            hashMap,
            optionalInt,
            optionalBool,
            optionalOneOf,
            optionalStruct,
            optionalEnum }

    write Example {
        oneOf,
        hashMap,
        optionalInt,
        optionalBool,
        optionalOneOf,
        optionalStruct,
        optionalEnum } = do
            Trans.write oneOf
            Trans.write hashMap
            Trans.write optionalInt
            Trans.write optionalBool
            Trans.write optionalOneOf
            Trans.write optionalStruct
            Trans.write optionalEnum