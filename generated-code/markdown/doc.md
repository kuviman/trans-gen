## `EntityType`

Entity type

Variants:

- `Wall` - Wall, can be used to prevent enemy from moving through
- `House` - House, used to increase population
- `BuilderBase` - Base for recruiting new builder units
- `BuilderUnit` - Builder unit can build buildings
- `MeleeBase` - Base for recruiting new melee units
- `MeleeUnit` - Melee unit
- `RangedBase` - Base for recruiting new ranged units
- `RangedUnit` - Ranged unit
- `Resource` - Resource can be harvested
- `Turret` - Ranged attacking building

## `BuildProperties`

Entity's build properties

Fields:

- `options`: `[EntityType]` - Valid new entity types
- `init_health`: `Option<int32>` - Initial health of new entity. If absent, it will have full health

## `AttackProperties`

Entity's attack properties

Fields:

- `attack_range`: `int32` - Maximum attack range
- `damage`: `int32` - Damage dealt in one tick
- `collect_resource`: `boolean` - If true, dealing damage will collect resource from target

## `RepairProperties`

Entity's repair properties

Fields:

- `valid_targets`: `[EntityType]` - Valid target entity types
- `power`: `int32` - Health restored in one tick

## `EntityProperties`

Entity properties

Fields:

- `size`: `int32` - Size. Entity has a form of a square with side of this length
- `build_score`: `int32` - Score for building this entity
- `destroy_score`: `int32` - Score for destroying this entity
- `can_move`: `boolean` - Whether this entity can move
- `population_provide`: `int32` - Number of population points this entity provides, if active
- `population_use`: `int32` - Number of population points this entity uses
- `max_health`: `int32` - Maximum health points
- `initial_cost`: `int32` - Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
- `sight_range`: `int32` - If fog of war is enabled, maximum distance at which other entities are considered visible
- `resource_per_health`: `int32` - Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
- `build`: `Option<BuildProperties>` - Build properties, if entity can build
- `attack`: `Option<AttackProperties>` - Attack properties, if entity can attack
- `repair`: `Option<RepairProperties>` - Repair properties, if entity can repair

## `Player`

Player (strategy, client)

Fields:

- `id`: `int32` - Player's ID
- `score`: `int32` - Current score
- `resource`: `int32` - Current amount of resource

## `Vec2Int32`

2 dimensional vector.

Fields:

- `x`: `int32` - `x` coordinate of the vector
- `y`: `int32` - `y` coordinate of the vector

## `Entity`

Game entity

Fields:

- `id`: `int32` - Entity's ID. Unique for each entity
- `player_id`: `Option<int32>` - Entity's owner player ID, if owned by a player
- `entity_type`: `EntityType` - Entity's type
- `position`: `Vec2Int32` - Entity's position (corner with minimal coordinates)
- `health`: `int32` - Current health
- `active`: `boolean` - If entity is active, it can perform actions

## `PlayerView`

Information available to the player

Fields:

- `my_id`: `int32` - Your player's ID
- `map_size`: `int32` - Size of the map
- `fog_of_war`: `boolean` - Whether fog of war is enabled
- `entity_properties`: `Map<EntityType -> EntityProperties>` - Entity properties for each entity type
- `max_tick_count`: `int32` - Max tick count for the game
- `max_pathfind_nodes`: `int32` - Max pathfind nodes when performing pathfinding in the game simulator
- `current_tick`: `int32` - Current tick
- `players`: `[Player]` - List of players
- `entities`: `[Entity]` - List of entities
