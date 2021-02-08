use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use trans::Trans;

/// RGBA Color
#[trans_doc = "ru:Цвет в формате RGBA"]
#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize, Trans)]
#[trans(no_generics_in_name)]
pub struct Color<T> {
    /// Red component
    #[trans_doc = "ru:Компонента красного цвета"]
    pub r: T,
    /// Green component
    #[trans_doc = "ru:Компонента зеленого цвета"]
    pub g: T,
    /// Blue component
    #[trans_doc = "ru:Компонента синего цвета"]
    pub b: T,
    /// Alpha (opacity) component
    #[trans_doc = "ru:Комнонента видимости (непрозрачности)"]
    pub a: T,
}

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
#[trans(namespace = "model")]
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
#[trans(namespace = "model")]
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
#[trans(namespace = "model")]
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
#[trans(namespace = "model")]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct RepairAction {
    /// Target entity's ID
    #[trans_doc = "ru:ID цели"]
    pub target: Id,
}

/// Auto attack options
#[trans_doc = "ru:Настройки автоматической атаки"]
#[trans(namespace = "model")]
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
#[trans(namespace = "model")]
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
#[trans(namespace = "model")]
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
#[trans(namespace = "model")]
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
#[trans(namespace = "model")]
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
#[trans(namespace = "model")]
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
#[trans(namespace = "model")]
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
#[trans(namespace = "model")]
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
#[trans(namespace = "model")]
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
#[trans(namespace = "model")]
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

/// Message sent from client
#[trans_doc = "ru:Сообщение отправляемое клиентом"]
#[trans(namespace = "codegame")]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
#[trans(no_generics_in_name)]
pub enum ClientMessage<G: Game> {
    /// Ask app to perform new debug command
    #[trans_doc = "ru:Отправить отладочную команду приложению"]
    DebugMessage {
        /// Command to perform
        #[trans_doc = "ru:Команда для исполнения"]
        #[serde(bound = "")]
        command: DebugCommand<G>,
    },
    /// Reply for ServerMessage::GetAction
    #[trans_doc = "ru:Ответ на ServerMessage::GetAction"]
    ActionMessage {
        /// Player's action
        #[trans_doc = "ru:Действие игрока"]
        action: G::Action,
    },
    /// Signifies finish of the debug update
    #[trans_doc = "ru:Сигнализирует окончание отладочного обновления"]
    DebugUpdateDone {},
    /// Request debug state from the app
    #[trans_doc = "ru:Запросить отладочное состояние приложения"]
    RequestDebugState {},
}

/// Message sent from server
#[trans_doc = "ru:Сообщение отправляемое сервером"]
#[trans(namespace = "codegame")]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
#[trans(no_generics_in_name)]
pub enum ServerMessage<G: Game> {
    /// Get action for next tick
    #[trans_doc = "ru:Получить действие для следующего тика"]
    GetAction {
        /// Player's view
        #[trans_doc = "ru:Информация доступная игроку"]
        player_view: G::PlayerView,
        /// Whether app is running with debug interface available
        #[trans_doc = "ru:Доступен ли отладочный интерфейс приложения"]
        debug_available: bool,
    },
    /// Signifies end of the game
    #[trans_doc = "ru:Сигнализирует конец игры"]
    Finish {},
    /// Debug update
    #[trans_doc = "ru:Отладочное обновление"]
    DebugUpdate {
        /// Player's view
        #[trans_doc = "ru:Информация доступная игроку"]
        player_view: G::PlayerView,
    },
}

/// Debug commands that can be sent while debugging with the app
#[trans_doc = "ru:Команды, которые могут быть отправлены приложению для помощи в отладке"]
#[trans(namespace = "codegame")]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
#[trans(no_generics_in_name)]
pub enum DebugCommand<G: Game> {
    /// Add debug data to current tick
    #[trans_doc = "ru:Добавить отладочные данные в текущий тик"]
    Add {
        /// Data to add
        #[trans_doc = "ru:Данные для добавления"]
        debug_data: G::DebugData,
    },
    /// Clear current tick's debug data
    #[trans_doc = "ru:Очистить отладочные данные текущего тика"]
    Clear,
    /// Enable/disable auto performing of commands
    #[trans_doc = "ru:Включить/выключить автоматическое выполнение команд"]
    SetAutoFlush {
        /// Enable/disable autoflush
        #[trans_doc = "ru:Включить/выключить автоматическое выполнение"]
        enable: bool,
    },
    /// Perform all previously sent commands
    #[trans_doc = "ru:Выполнить все присланные ранее команды"]
    Flush,
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Trans)]
#[trans(namespace = "model::debug_interface")]
/// Primitive type for debug rendering
#[trans_doc = "ru:Тип примитивов для отладочной отрисовки"]
pub enum PrimitiveType {
    /// Lines, number of vertices should be divisible by 2
    #[trans_doc = "ru:Линии, количество вершин должно делиться на 2"]
    Lines,
    /// Triangles, number of vertices should be divisible by 3
    #[trans_doc = "ru:Треугольники, количество вершин должно делиться на 3"]
    Triangles,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
/// Vertex for debug rendering
#[trans_doc = "ru:Вершина для отладочной отрисовки"]
#[trans(namespace = "model::debug_interface")]
pub struct ColoredVertex {
    /// Position in world coordinates (if none, screen position (0, 0) is used)
    #[trans_doc = "ru:Позиция в мировых координатах (если отсутствует, используются координаты (0, 0) экрана)"]
    pub world_pos: Option<Vec2<f32>>,
    /// Additional offset in screen coordinates
    #[trans_doc = "ru:Дополнительное смещение в экранных координатах"]
    pub screen_offset: Vec2<f32>,
    /// Color to use
    #[trans_doc = "ru:Цвет"]
    pub color: Color<f32>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
/// Debug data can be drawn in the app
#[trans_doc = "ru:Данные для отладки, которые могут быть отображены при помощи приложения"]
#[trans(namespace = "model::debug_interface")]
pub enum DebugData {
    /// Log some text
    #[trans_doc = "ru:Добавить запись в лог"]
    Log {
        /// Text to show
        #[trans_doc = "ru:Текст лога"]
        text: String,
    },
    /// Draw primitives
    #[trans_doc = "ru:Отрисовка примитивов"]
    Primitives {
        /// Vertices
        #[trans_doc = "ru:Вершины"]
        vertices: Vec<ColoredVertex>,
        /// Primitive type
        #[trans_doc = "ru:Тип примитивов"]
        primitive_type: PrimitiveType,
    },
    /// Draw text
    #[trans_doc = "ru:Отрисовка текста"]
    PlacedText {
        /// Vertex to determine text position and color
        #[trans_doc = "ru:Вершина для определения положения и цвета текста"]
        vertex: ColoredVertex,
        /// Text
        #[trans_doc = "ru:Текст"]
        text: String,
        /// Text alignment (0 means left, 0.5 means center, 1 means right)
        #[trans_doc = "ru:Выравнивание (0 - по левому краю, 0.5 - по центру, 1 - по правому краю)"]
        alignment: f32,
        /// Font size in pixels
        #[trans_doc = "ru: Размер шрифта в пикселях"]
        size: f32,
    },
}

/// Debug state to be received from the app
#[trans_doc = "ru:Состояние для отладки, получаемое из приложения"]
#[trans(namespace = "model::debug_interface")]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct DebugState {
    /// Size of the drawing canvas
    #[trans_doc = "ru:Размер окна для отрисовки"]
    pub window_size: Vec2<usize>,
    /// Mouse position in window coordinates
    #[trans_doc = "ru:Положение курсора в оконных координатах"]
    pub mouse_pos_window: Vec2<f32>,
    /// Mouse position in world coordinates
    #[trans_doc = "ru:Положение курсора в мировых координатах"]
    pub mouse_pos_world: Vec2<f32>,
    /// Currently pressed keys
    #[trans_doc = "ru:Кнопки, нажатые в данный момент"]
    pub pressed_keys: Vec<String>,
    /// Current camera used for rendering
    #[trans_doc = "ru:Текущая камера используемая для отрисовки"]
    pub camera: Camera,
    /// Your player's index
    #[trans_doc = "ru:Индекс вашего игрока"]
    pub player_index: usize,
}

/// Camera used for rendering
#[trans_doc = "ru:Камера используемая для отрисовки"]
#[trans(namespace = "model::debug_interface")]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct Camera {
    /// Center point at which camera is looking
    #[trans_doc = "ru:Точка на которую смотрит камера"]
    pub center: Vec2<f32>,
    /// Rotation angle
    #[trans_doc = "ru:Угол поворота"]
    pub rotation: f32,
    /// Attack angle
    #[trans_doc = "ru:Угол атаки"]
    pub attack: f32,
    /// Distance to center
    #[trans_doc = "ru:Расстояние до цели"]
    pub distance: f32,
    /// Whether perspective is applied
    #[trans_doc = "ru:Применяется ли перспектива"]
    pub perspective: bool,
}

/// Client or server message
#[trans_doc = "ru:Сообщение клиента или сервера"]
#[trans(namespace = "codegame")]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub enum Message<G: Game> {
    /// Client message
    #[trans_doc = "ru:Сообщение клиента"]
    Client {
        /// Message
        #[trans_doc = "ru:Сообщение"]
        #[serde(bound = "")]
        message: ClientMessage<G>,
    },
    /// Server message
    #[trans_doc = "ru:Сообщение сервера"]
    Server {
        /// Message
        #[trans_doc = "ru:Сообщение"]
        #[serde(bound = "")]
        message: ServerMessage<G>,
    },
}

/// Player's action
#[trans_doc = "ru:Действие игрока"]
#[trans(namespace = "model")]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct Action {
    /// New actions for entities. If entity does not get new action, if will continue to perform previously set one
    #[trans_doc = "ru:Новые действия для сущностей. Если сущность не получила новое действие, она будет продолжать выполнять предыдущее"]
    pub entity_actions: HashMap<Id, EntityAction>,
}

/// Game model
#[trans_doc = "ru:Игровая модель"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct GameModel {}

pub trait Game:
    PartialEq
    + std::fmt::Debug
    + Serialize
    + for<'de> Deserialize<'de>
    + Trans
    + Sync
    + Send
    + Clone
    + 'static
{
    type Action: PartialEq
        + std::fmt::Debug
        + Serialize
        + for<'de> Deserialize<'de>
        + Trans
        + Sync
        + Send
        + Clone
        + 'static;
    type PlayerView: PartialEq
        + std::fmt::Debug
        + Serialize
        + for<'de> Deserialize<'de>
        + Trans
        + Sync
        + Send
        + Clone
        + 'static;
    type DebugData: PartialEq
        + std::fmt::Debug
        + Serialize
        + for<'de> Deserialize<'de>
        + Trans
        + Sync
        + Send
        + Clone
        + 'static;
    type DebugState: PartialEq
        + std::fmt::Debug
        + Serialize
        + for<'de> Deserialize<'de>
        + Trans
        + Sync
        + Send
        + Clone
        + 'static;
}

impl Game for GameModel {
    type Action = Action;
    type PlayerView = PlayerView;
    type DebugData = DebugData;
    type DebugState = DebugState;
}

pub type Model = Message<GameModel>;
pub const SHOW_STDOUT: bool = false;
pub fn version() -> trans::Version {
    trans::version()
}
