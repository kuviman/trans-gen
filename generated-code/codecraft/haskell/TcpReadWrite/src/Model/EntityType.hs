module Model.EntityType where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Data.Int

-- | Entity type
data EntityType
    -- | Wall, can be used to prevent enemy from moving through
    = Wall
    -- | House, used to increase population
    | House
    -- | Base for recruiting new builder units
    | BuilderBase
    -- | Builder unit can build buildings
    | BuilderUnit
    -- | Base for recruiting new melee units
    | MeleeBase
    -- | Melee unit
    | MeleeUnit
    -- | Base for recruiting new ranged units
    | RangedBase
    -- | Ranged unit
    | RangedUnit
    -- | Resource can be harvested
    | Resource
    -- | Ranged attacking building
    | Turret
    deriving (Eq, Ord, Show)

instance Trans EntityType where
    read = do
        tag :: Int32 <- Trans.read
        return $ case tag of
            0 -> Wall
            1 -> House
            2 -> BuilderBase
            3 -> BuilderUnit
            4 -> MeleeBase
            5 -> MeleeUnit
            6 -> RangedBase
            7 -> RangedUnit
            8 -> Resource
            9 -> Turret

    write Wall =
        Trans.write (0 :: Int32)
    write House =
        Trans.write (1 :: Int32)
    write BuilderBase =
        Trans.write (2 :: Int32)
    write BuilderUnit =
        Trans.write (3 :: Int32)
    write MeleeBase =
        Trans.write (4 :: Int32)
    write MeleeUnit =
        Trans.write (5 :: Int32)
    write RangedBase =
        Trans.write (6 :: Int32)
    write RangedUnit =
        Trans.write (7 :: Int32)
    write Resource =
        Trans.write (8 :: Int32)
    write Turret =
        Trans.write (9 :: Int32)