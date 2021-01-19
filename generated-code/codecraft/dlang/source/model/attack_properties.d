import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct AttackProperties {
    int attackRange;
    int damage;
    bool collectResource;

    this(int attackRange, int damage, bool collectResource) {
        this.attackRange = attackRange;
        this.damage = damage;
        this.collectResource = collectResource;
    }

    static AttackProperties readFrom(Stream reader) {
        int attackRange;
        attackRange = reader.readInt();
        int damage;
        damage = reader.readInt();
        bool collectResource;
        collectResource = reader.readBool();
        return AttackProperties(attackRange, damage, collectResource);
    }

    void writeTo(Stream writer) const {
        writer.write(attackRange);
        writer.write(damage);
        writer.write(collectResource);
    }
}