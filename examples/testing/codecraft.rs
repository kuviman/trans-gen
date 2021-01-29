use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use trans::Trans;

/// 2 dimensional vector.
#[trans_doc = "ru:Двумерный вектор"]
#[repr(C)]
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Serialize, Deserialize, Trans)]
pub struct Vec2<T> {
    /// `x` coordinate of the vector
    #[trans_doc = "ru:Координата `x` вектора"]
    pub x: T,
    /// `y` coordinate of the vector
    #[trans_doc = "ru:Координата `y` вектора"]
    pub y: T,
}

#[derive(Debug, Serialize, Deserialize, Clone, Hash, PartialEq, Eq, Copy, Trans)]
pub struct Id(usize);

/// Game entity
#[trans_doc = "ru:Игровая сущность"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct Entity {
    /// Entity's ID. Unique for each entity
    #[trans_doc = "ru:ID сущности. Уникально для каждой сущности"]
    pub id: Id,
    /// Entity's owner player ID, if owned by a player
    #[trans_doc = "ru:ID игрока, владеющего сущностью, если применимо"]
    pub player_id: Option<Id>,
    /// Entity's type
    #[trans_doc = "ru:Тип сущности"]
    pub entity_type: EntityType,
    /// Entity's position (corner with minimal coordinates)
    #[trans_doc = "ru:Позиция сущности (угол с минимальными координатами)"]
    pub position: Vec2<usize>,
    /// Current health
    #[trans_doc = "ru:Текущее здоровье"]
    pub health: usize,
    /// If entity is active, it can perform actions
    #[trans_doc = "ru:Если сущность активна, она может выполнять действия"]
    pub active: bool,
}

/// Move action
#[trans_doc = "ru:Действие перемещения"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct MoveAction {
    /// Target position
    #[trans_doc = "ru:Целевая позиция"]
    pub target: Vec2<usize>,
    /// Whether to try find closest position, if path to target is not found
    #[trans_doc = "ru:Находить ли ближайшее положение, если до цели путь не найден"]
    pub find_closest_position: bool,
    /// Whether to destroy other entities on the way
    #[trans_doc = "ru:Уничтожать ли враждебные сущности на пути"]
    pub break_through: bool,
}

/// Build action
#[trans_doc = "ru:Действие постройки"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct BuildAction {
    /// Type of an entity to build
    #[trans_doc = "ru:Тип сущности для постройки"]
    pub entity_type: EntityType,
    /// Desired position of new entity
    #[trans_doc = "ru:Желаемая позиция новой сущности"]
    pub position: Vec2<usize>,
}

/// Repair action
#[trans_doc = "ru:Действие починки"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct RepairAction {
    /// Target entity's ID
    #[trans_doc = "ru:ID цели"]
    pub target: Id,
}

/// Auto attack options
#[trans_doc = "ru:Настройки автоматической атаки"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct AutoAttack {
    /// Maximum distance to pathfind
    #[trans_doc = "ru:Максимальное расстояние для поиска пути"]
    pub pathfind_range: usize,
    /// List of target entity types to try to attack. If empty, all types but resource are considered
    #[trans_doc = "ru:Список типов сущностей, которые следует атаковать. Если пусто, все типы кроме ресурса будут рассмотрены"]
    pub valid_targets: Vec<EntityType>,
}

/// Attack action
#[trans_doc = "ru:Действие атаки"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct AttackAction {
    /// If specified, target entity's ID
    #[trans_doc = "ru:ID цели, если применимо"]
    pub target: Option<Id>,
    /// If specified, configures auto attacking
    #[trans_doc = "ru:Настройки автоматической атаки, если необходимо"]
    pub auto_attack: Option<AutoAttack>,
}

/// Entity's action
#[trans_doc = "ru:Действие сущности"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct EntityAction {
    /// Move action
    #[trans_doc = "ru:Действие перемещения"]
    pub move_action: Option<MoveAction>,
    /// Build action
    #[trans_doc = "ru:Действие постройки"]
    pub build_action: Option<BuildAction>,
    /// Attack action
    #[trans_doc = "ru:Действие атаки"]
    pub attack_action: Option<AttackAction>,
    /// Repair action
    #[trans_doc = "ru:Действие починки"]
    pub repair_action: Option<RepairAction>,
}

/// Entity's attack properties
#[trans_doc = "ru:Свойства атаки сущности"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct AttackProperties {
    /// Maximum attack range
    #[trans_doc = "ru:Максимальное расстояние атаки"]
    #[trans(rename = "attack_range")]
    #[serde(rename = "attack_range")]
    pub range: usize,
    /// Damage dealt in one tick
    #[trans_doc = "ru:Урон наносимый за один тик"]
    pub damage: usize,
    /// If true, dealing damage will collect resource from target
    #[trans_doc = "ru:Собираются ли ресурсы с цели при атаке"]
    pub collect_resource: bool,
}

/// Entity's build properties
#[trans_doc = "ru:Свойства строительства сущности"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct BuildProperties {
    /// Valid new entity types
    #[trans_doc = "ru:Возможные типы новой сущности"]
    pub options: Vec<EntityType>,
    /// Initial health of new entity. If absent, it will have full health
    #[trans_doc = "ru:Изначальное здоровье новой сущности. Если отсутствует, новая сущность будет имет полное здоровье"]
    pub init_health: Option<usize>,
}

/// Entity's repair properties
#[trans_doc = "ru:Свойства ремонта сущности"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct RepairProperties {
    /// Valid target entity types
    #[trans_doc = "ru:Типы сущностей, которые возможно ремонтировать"]
    pub valid_targets: Vec<EntityType>,
    /// Health restored in one tick
    #[trans_doc = "ru:Здоровье восстанавливаемое за один тик"]
    pub power: usize,
}

/// Entity properties
#[trans_doc = "ru:Свойства сущности"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct EntityProperties {
    /// Size. Entity has a form of a square with side of this length
    #[trans_doc = "ru:Размер. Сущности имеют форму квадрата со стороной заданной длины"]
    pub size: usize,
    /// Score for building this entity
    #[trans_doc = "ru:Количество очков за постройку данной сущности"]
    pub build_score: usize,
    /// Score for destroying this entity
    #[trans_doc = "ru:Количество очков за уничтожение данной сущности"]
    pub destroy_score: usize,
    /// Whether this entity can move
    #[trans_doc = "ru:Может ли данная сущность перемещаться"]
    pub can_move: bool,
    /// Number of population points this entity provides, if active
    #[trans_doc = "ru:Количество производимой еды, если сущность активна"]
    pub population_provide: usize,
    /// Number of population points this entity uses
    #[trans_doc = "ru:Количество потребляемой еды"]
    pub population_use: usize,
    /// Maximum health points
    #[trans_doc = "ru:Максимальное количество очков здоровья"]
    pub max_health: usize,
    /// Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
    #[trans_doc = "ru:Стоимость постройки первой сущности данного типа. Если это юнит (сущность может перемещаться), стоимость увеличивается на 1 за каждого существующего юнита этого типа"]
    pub initial_cost: usize,
    /// If fog of war is enabled, maximum distance at which other entities are considered visible
    #[trans_doc = "ru:Если включен туман войны, расстояние на котором другие сущности считаются видимыми"]
    pub sight_range: usize,
    /// Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
    #[trans_doc = "ru:Количество ресурса добавляемое нападающему, способному собирать русурсы, за каждую единицу урона"]
    pub resource_per_health: usize,
    /// Build properties, if entity can build
    #[trans_doc = "ru:Свойства строительства, если сущность способна строить"]
    pub build: Option<BuildProperties>,
    /// Attack properties, if entity can attack
    #[trans_doc = "ru:Свойства атаки, если сущность способна атаковать"]
    pub attack: Option<AttackProperties>,
    /// Repair properties, if entity can repair
    #[trans_doc = "ru:Свойства ремонта, если сущность способна ремонтировать"]
    pub repair: Option<RepairProperties>,
}

/// Entity type
#[trans_doc = "ru:Тип сущности"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Eq, Hash, Copy, Clone)]
pub enum EntityType {
    /// Wall, can be used to prevent enemy from moving through
    #[trans_doc = "ru:Стена, может использоваться для блокировки пути противнику"]
    Wall,
    /// House, used to increase population
    #[trans_doc = "ru:Дом, производит еду"]
    House,
    /// Base for recruiting new builder units
    #[trans_doc = "ru:База для покупки юнитов-строителей"]
    BuilderBase,
    /// Builder unit can build buildings
    #[trans_doc = "ru:Юнит-строитель может строить здания"]
    BuilderUnit,
    /// Base for recruiting new melee units
    #[trans_doc = "ru:База для покупки юнитов ближнего боя"]
    MeleeBase,
    /// Melee unit
    #[trans_doc = "ru:Юнит ближнего боя"]
    MeleeUnit,
    /// Base for recruiting new ranged units
    #[trans_doc = "ru:База для покупки юнитов дальнего боя"]
    RangedBase,
    /// Ranged unit
    #[trans_doc = "ru:Юнит дальнего боя"]
    RangedUnit,
    /// Resource can be harvested
    #[trans_doc = "ru:Ресурс, может быть собран"]
    Resource,
    /// Ranged attacking building
    #[trans_doc = "ru:Здание способное атаковать на расстоянии"]
    Turret,
}

/// Player (strategy, client)
#[trans_doc = "ru:Игрок (стратегия, клиент)"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct Player {
    /// Player's ID
    #[trans_doc = "ru:ID игрока"]
    pub id: Id,
    /// Current score
    #[trans_doc = "ru:Текущий счет"]
    pub score: usize,
    /// Current amount of resource
    #[trans_doc = "ru:Текущее количество ресурса"]
    pub resource: usize,
}

/// Information available to the player
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
#[trans_doc = "ru:Доступная игроку информация"]
pub struct PlayerView {
    /// Your player's ID
    #[trans_doc = "ru:ID вашего игрока"]
    pub my_id: Id,
    /// Size of the map
    #[trans_doc = "ru:Размер карты"]
    pub map_size: usize,
    /// Whether fog of war is enabled
    #[trans_doc = "ru:Включен ли туман войны"]
    pub fog_of_war: bool,
    /// Entity properties for each entity type
    #[trans_doc = "ru:Свойства сущностей для каждого типа"]
    pub entity_properties: HashMap<EntityType, EntityProperties>,
    /// Max tick count for the game
    #[trans_doc = "ru:Максимальная длительность игры в тиках"]
    pub max_tick_count: usize,
    /// Max pathfind nodes when performing pathfinding in the game simulator
    #[trans_doc = "ru:Максимальное количество вершин для поиска пути в игровом симуляторе"]
    pub max_pathfind_nodes: usize,
    /// Current tick
    #[trans_doc = "ru:Текущий тик"]
    pub current_tick: usize,
    /// List of players
    #[trans_doc = "ru:Список игроков"]
    pub players: Vec<Player>,
    /// List of entities
    #[trans_doc = "ru:Список сущностей"]
    pub entities: Vec<Entity>,
}

pub type Model = PlayerView;
pub const SHOW_STDOUT: bool = false;
pub fn version() -> trans::Version {
    trans::version()
}
