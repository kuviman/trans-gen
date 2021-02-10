## Common

### `Vec2Float32`

2 dimensional vector.

Fields:

* `x`: `float32` &mdash; `x` coordinate of the vector
* `y`: `float32` &mdash; `y` coordinate of the vector

### `Color`

RGBA Color

Fields:

* `r`: `float32` &mdash; Red component
* `g`: `float32` &mdash; Green component
* `b`: `float32` &mdash; Blue component
* `a`: `float32` &mdash; Alpha (opacity) component

### `Vec2Int32`

2 dimensional vector.

Fields:

* `x`: `int32` &mdash; `x` coordinate of the vector
* `y`: `int32` &mdash; `y` coordinate of the vector

## Codegame

### `DebugCommand`

Debug commands that can be sent while debugging with the app

One of:

* `Add` &mdash; Add debug data to current tick

  Fields:

  + `debug_data`: `Model::DebugInterface::DebugData` &mdash; Data to add
* `Clear` &mdash; Clear current tick's debug data

  No fields
* `SetAutoFlush` &mdash; Enable/disable auto performing of commands

  Fields:

  + `enable`: `boolean` &mdash; Enable/disable autoflush
* `Flush` &mdash; Perform all previously sent commands

  No fields

### `ClientMessage`

Message sent from client

One of:

* `DebugMessage` &mdash; Ask app to perform new debug command

  Fields:

  + `command`: `DebugCommand` &mdash; Command to perform
* `ActionMessage` &mdash; Reply for ServerMessage::GetAction

  Fields:

  + `action`: `Model::Action` &mdash; Player's action
* `DebugUpdateDone` &mdash; Signifies finish of the debug update

  No fields
* `RequestDebugState` &mdash; Request debug state from the app

  No fields

### `ServerMessage`

Message sent from server

One of:

* `GetAction` &mdash; Get action for next tick

  Fields:

  + `player_view`: `Model::PlayerView` &mdash; Player's view
  + `debug_available`: `boolean` &mdash; Whether app is running with debug interface available
* `Finish` &mdash; Signifies end of the game

  No fields
* `DebugUpdate` &mdash; Debug update

  Fields:

  + `player_view`: `Model::PlayerView` &mdash; Player's view

### `MessageGameModel`

Client or server message

One of:

* `Client` &mdash; Client message

  Fields:

  + `message`: `ClientMessage` &mdash; Message
* `Server` &mdash; Server message

  Fields:

  + `message`: `ServerMessage` &mdash; Message

## Model

### `MoveAction`

Move action

Fields:

* `target`: `Vec2Int32` &mdash; Target position
* `find_closest_position`: `boolean` &mdash; Whether to try find closest position, if path to target is not found
* `break_through`: `boolean` &mdash; Whether to destroy other entities on the way

### `EntityType`

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

### `BuildAction`

Build action

Fields:

* `entity_type`: `EntityType` &mdash; Type of an entity to build
* `position`: `Vec2Int32` &mdash; Desired position of new entity

### `AutoAttack`

Auto attack options

Fields:

* `pathfind_range`: `int32` &mdash; Maximum distance to pathfind
* `valid_targets`: `[EntityType]` &mdash; List of target entity types to try to attack. If empty, all types but resource are considered

### `AttackAction`

Attack action

Fields:

* `target`: `Option<int32>` &mdash; If specified, target entity's ID
* `auto_attack`: `Option<AutoAttack>` &mdash; If specified, configures auto attacking

### `RepairAction`

Repair action

Fields:

* `target`: `int32` &mdash; Target entity's ID

### `EntityAction`

Entity's action

Fields:

* `move_action`: `Option<MoveAction>` &mdash; Move action
* `build_action`: `Option<BuildAction>` &mdash; Build action
* `attack_action`: `Option<AttackAction>` &mdash; Attack action
* `repair_action`: `Option<RepairAction>` &mdash; Repair action

### `Action`

Player's action

Fields:

* `entity_actions`: `Map<int32 -> EntityAction>` &mdash; New actions for entities. If entity does not get new action, if will continue to perform previously set one

### `BuildProperties`

Entity's build properties

Fields:

* `options`: `[EntityType]` &mdash; Valid new entity types
* `init_health`: `Option<int32>` &mdash; Initial health of new entity. If absent, it will have full health

### `AttackProperties`

Entity's attack properties

Fields:

* `attack_range`: `int32` &mdash; Maximum attack range
* `damage`: `int32` &mdash; Damage dealt in one tick
* `collect_resource`: `boolean` &mdash; If true, dealing damage will collect resource from target

### `RepairProperties`

Entity's repair properties

Fields:

* `valid_targets`: `[EntityType]` &mdash; Valid target entity types
* `power`: `int32` &mdash; Health restored in one tick

### `EntityProperties`

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

### `Player`

Player (strategy, client)

Fields:

* `id`: `int32` &mdash; Player's ID
* `score`: `int32` &mdash; Current score
* `resource`: `int32` &mdash; Current amount of resource

### `Entity`

Game entity

Fields:

* `id`: `int32` &mdash; Entity's ID. Unique for each entity
* `player_id`: `Option<int32>` &mdash; Entity's owner player ID, if owned by a player
* `entity_type`: `EntityType` &mdash; Entity's type
* `position`: `Vec2Int32` &mdash; Entity's position (corner with minimal coordinates)
* `health`: `int32` &mdash; Current health
* `active`: `boolean` &mdash; If entity is active, it can perform actions

### `PlayerView`

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

## Model::DebugInterface

### `ColoredVertex`

Vertex for debug rendering

Fields:

* `world_pos`: `Option<Vec2Float32>` &mdash; Position in world coordinates (if none, screen position (0, 0) is used)
* `screen_offset`: `Vec2Float32` &mdash; Additional offset in screen coordinates
* `color`: `Color` &mdash; Color to use

### `PrimitiveType`

Primitive type for debug rendering

Variants:

* `Lines` &mdash; Lines, number of vertices should be divisible by 2
* `Triangles` &mdash; Triangles, number of vertices should be divisible by 3

### `DebugData`

Debug data can be drawn in the app

One of:

* `Log` &mdash; Log some text

  Fields:

  + `text`: `string` &mdash; Text to show
* `Primitives` &mdash; Draw primitives

  Fields:

  + `vertices`: `[ColoredVertex]` &mdash; Vertices
  + `primitive_type`: `PrimitiveType` &mdash; Primitive type
* `PlacedText` &mdash; Draw text

  Fields:

  + `vertex`: `ColoredVertex` &mdash; Vertex to determine text position and color
  + `text`: `string` &mdash; Text
  + `alignment`: `float32` &mdash; Text alignment (0 means left, 0.5 means center, 1 means right)
  + `size`: `float32` &mdash; Font size in pixels