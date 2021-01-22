package model;

import util.StreamUtil;

public class AttackProperties {
    private int attackRange;

    public int getAttackRange() {
        return attackRange;
    }

    public void setAttackRange(int value) {
        this.attackRange = value;
    }
    private int damage;

    public int getDamage() {
        return damage;
    }

    public void setDamage(int value) {
        this.damage = value;
    }
    private boolean collectResource;

    public boolean isCollectResource() {
        return collectResource;
    }

    public void setCollectResource(boolean value) {
        this.collectResource = value;
    }

    public AttackProperties(int attackRange, int damage, boolean collectResource) {
        this.attackRange = attackRange;
        this.damage = damage;
        this.collectResource = collectResource;
    }

    public static AttackProperties readFrom(java.io.InputStream stream) throws java.io.IOException {
        int attackRange;
        attackRange = StreamUtil.readInt(stream);
        int damage;
        damage = StreamUtil.readInt(stream);
        boolean collectResource;
        collectResource = StreamUtil.readBoolean(stream);
        return new AttackProperties(attackRange, damage, collectResource);
    }

    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, attackRange);
        StreamUtil.writeInt(stream, damage);
        StreamUtil.writeBoolean(stream, collectResource);
    }

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