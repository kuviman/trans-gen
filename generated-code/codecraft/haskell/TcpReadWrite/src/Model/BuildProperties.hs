module Model.BuildProperties where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int
import Model.EntityType (EntityType)

data BuildProperties = BuildProperties {
    options :: [EntityType],
    initHealth :: Maybe Int32 }
    deriving Show

instance Trans BuildProperties where
    read = do
        options <- Trans.read
        initHealth <- Trans.read
        return BuildProperties {
            options,
            initHealth }

    write BuildProperties {
        options,
        initHealth } = do
            Trans.write options
            Trans.write initHealth