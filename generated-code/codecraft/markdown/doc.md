## `EntityType`

Entity type

Variants:

* `Wall` &mdash; Wall, can be used to prevent enemy from moving through
* `House` &mdash; House, used to increase population
* `BuilderBase` &mdash; Base for recruiting new builder units
* `BuilderUnit` &mdash; Builder unit can build buildings
* `MeleeBase` &mdash; Base for recruiting new melee units
* `MeleeUnit` &mdash; Melee unit
* `RangedBase` &mdash; Base for recruiting new ranged units
* `RangedUnit` &mdash; Ranged unit
* `Resource` &mdash; Resource can be harvested
* `Turret` &mdash; Ranged attacking building

## `BuildProperties`

Entity's build properties

Fields:

* `options`: `[EntityType]` &mdash; Valid new entity types
* `init_health`: `Option<int32>` &mdash; Initial health of new entity. If absent, it will have full health

## `AttackProperties`

Entity's attack properties

Fields:

* `attack_range`: `int32` &mdash; Maximum attack range
* `damage`: `int32` &mdash; Damage dealt in one tick
* `collect_resource`: `boolean` &mdash; If true, dealing damage will collect resource from target

## `RepairProperties`

Entity's repair properties

Fields:

* `valid_targets`: `[EntityType]` &mdash; Valid target entity types
* `power`: `int32` &mdash; Health restored in one tick

## `EntityProperties`

Entity properties

Fields:

* `size`: `int32` &mdash; Size. Entity has a form of a square with side of this length
* `build_score`: `int32` &mdash; Score for building this entity
* `destroy_score`: `int32` &mdash; Score for destroying this entity
* `can_move`: `boolean` &mdash; Whether this entity can move
* `population_provide`: `int32` &mdash; Number of population points this entity provides, if active
* `population_use`: `int32` &mdash; Number of population points this entity uses
* `max_health`: `int32` &mdash; Maximum health points
* `initial_cost`: `int32` &mdash; Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
* `sight_range`: `int32` &mdash; If fog of war is enabled, maximum distance at which other entities are considered visible
* `resource_per_health`: `int32` &mdash; Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
* `build`: `Option<BuildProperties>` &mdash; Build properties, if entity can build
* `attack`: `Option<AttackProperties>` &mdash; Attack properties, if entity can attack
* `repair`: `Option<RepairProperties>` &mdash; Repair properties, if entity can repair

## `Player`

Player (strategy, client)

Fields:

* `id`: `int32` &mdash; Player's ID
* `score`: `int32` &mdash; Current score
* `resource`: `int32` &mdash; Current amount of resource

## `Vec2Int32`

2 dimensional vector.

Fields:

* `x`: `int32` &mdash; `x` coordinate of the vector
* `y`: `int32` &mdash; `y` coordinate of the vector

## `Entity`

Game entity

Fields:

* `id`: `int32` &mdash; Entity's ID. Unique for each entity
* `player_id`: `Option<int32>` &mdash; Entity's owner player ID, if owned by a player
* `entity_type`: `EntityType` &mdash; Entity's type
* `position`: `Vec2Int32` &mdash; Entity's position (corner with minimal coordinates)
* `health`: `int32` &mdash; Current health
* `active`: `boolean` &mdash; If entity is active, it can perform actions

## `PlayerView`

Information available to the player

Fields:

* `my_id`: `int32` &mdash; Your player's ID
* `map_size`: `int32` &mdash; Size of the map
* `fog_of_war`: `boolean` &mdash; Whether fog of war is enabled
* `entity_properties`: `Map<EntityType -> EntityProperties>` &mdash; Entity properties for each entity type
* `max_tick_count`: `int32` &mdash; Max tick count for the game
* `max_pathfind_nodes`: `int32` &mdash; Max pathfind nodes when performing pathfinding in the game simulator
* `current_tick`: `int32` &mdash; Current tick
* `players`: `[Player]` &mdash; List of players
* `entities`: `[Entity]` &mdash; List of entities