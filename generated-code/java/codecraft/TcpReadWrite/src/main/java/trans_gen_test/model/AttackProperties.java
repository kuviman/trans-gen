package trans_gen_test.model;

import trans_gen_test.util.StreamUtil;

/**
 * Entity's attack properties
 */
public class AttackProperties {
    /**
     * Maximum attack range
     */
    private int attackRange;

    /**
     * Maximum attack range
     */
    public int getAttackRange() {
        return attackRange;
    }

    /**
     * Maximum attack range
     */
    public void setAttackRange(int value) {
        this.attackRange = value;
    }
    /**
     * Damage dealt in one tick
     */
    private int damage;

    /**
     * Damage dealt in one tick
     */
    public int getDamage() {
        return damage;
    }

    /**
     * Damage dealt in one tick
     */
    public void setDamage(int value) {
        this.damage = value;
    }
    /**
     * If true, dealing damage will collect resource from target
     */
    private boolean collectResource;

    /**
     * If true, dealing damage will collect resource from target
     */
    public boolean isCollectResource() {
        return collectResource;
    }

    /**
     * If true, dealing damage will collect resource from target
     */
    public void setCollectResource(boolean value) {
        this.collectResource = value;
    }

    public AttackProperties(int attackRange, int damage, boolean collectResource) {
        this.attackRange = attackRange;
        this.damage = damage;
        this.collectResource = collectResource;
    }

    /**
     * Read AttackProperties from input stream
     */
    public static AttackProperties readFrom(java.io.InputStream stream) throws java.io.IOException {
        int attackRange;
        attackRange = StreamUtil.readInt(stream);
        int damage;
        damage = StreamUtil.readInt(stream);
        boolean collectResource;
        collectResource = StreamUtil.readBoolean(stream);
        return new AttackProperties(attackRange, damage, collectResource);
    }

    /**
     * Write AttackProperties to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, attackRange);
        StreamUtil.writeInt(stream, damage);
        StreamUtil.writeBoolean(stream, collectResource);
    }

    /**
     * Get string representation of AttackProperties
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("AttackProperties { ");
        stringBuilder.append("attackRange: ");
        stringBuilder.append(String.valueOf(attackRange));
        stringBuilder.append(", ");
        stringBuilder.append("damage: ");
        stringBuilder.append(String.valueOf(damage));
        stringBuilder.append(", ");
        stringBuilder.append("collectResource: ");
        stringBuilder.append(String.valueOf(collectResource));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}