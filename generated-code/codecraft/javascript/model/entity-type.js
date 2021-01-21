module.exports = {
    Wall: 0,
    House: 1,
    BuilderBase: 2,
    BuilderUnit: 3,
    MeleeBase: 4,
    MeleeUnit: 5,
    RangedBase: 6,
    RangedUnit: 7,
    Resource: 8,
    Turret: 9,
    readFrom: async function(stream) {
        const result = stream.readInt();
        if (result < 0 || result >= 10) {
            throw new Error("Unexpected tag value");
        }
        return result;
    }
};