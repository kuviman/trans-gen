import model;
import stream;
import std.conv;
import std.typecons : Nullable;

/// Entity's attack properties
struct AttackProperties {
    /// Maximum attack range
    int attackRange;
    /// Damage dealt in one tick
    int damage;
    /// If true, dealing damage will collect resource from target
    bool collectResource;

    this(int attackRange, int damage, bool collectResource) {
        this.attackRange = attackRange;
        this.damage = damage;
        this.collectResource = collectResource;
    }

    /// Read AttackProperties from reader
    static AttackProperties readFrom(Stream reader) {
        int attackRange;
        attackRange = reader.readInt();
        int damage;
        damage = reader.readInt();
        bool collectResource;
        collectResource = reader.readBool();
        return AttackProperties(attackRange, damage, collectResource);
    }

    /// Write AttackProperties to writer
    void writeTo(Stream writer) const {
        writer.write(attackRange);
        writer.write(damage);
        writer.write(collectResource);
    }
}