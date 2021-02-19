unit EntityType;

interface

uses
    Stream,
    SysUtils;

type
    // Entity type
    {$scopedEnums on}
    TEntityType = (
        // Wall, can be used to prevent enemy from moving through
        Wall = 0,
        // House, used to increase population
        House = 1,
        // Base for recruiting new builder units
        BuilderBase = 2,
        // Builder unit can build buildings
        BuilderUnit = 3,
        // Base for recruiting new melee units
        MeleeBase = 4,
        // Melee unit
        MeleeUnit = 5,
        // Base for recruiting new ranged units
        RangedBase = 6,
        // Ranged unit
        RangedUnit = 7,
        // Resource can be harvested
        Resource = 8,
        // Ranged attacking building
        Turret = 9);

implementation

end.