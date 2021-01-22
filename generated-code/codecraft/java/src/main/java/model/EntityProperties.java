package model;

import util.StreamUtil;

public class EntityProperties {
    private int size;

    public int getSize() {
        return size;
    }

    public void setSize(int value) {
        this.size = value;
    }
    private int buildScore;

    public int getBuildScore() {
        return buildScore;
    }

    public void setBuildScore(int value) {
        this.buildScore = value;
    }
    private int destroyScore;

    public int getDestroyScore() {
        return destroyScore;
    }

    public void setDestroyScore(int value) {
        this.destroyScore = value;
    }
    private boolean canMove;

    public boolean isCanMove() {
        return canMove;
    }

    public void setCanMove(boolean value) {
        this.canMove = value;
    }
    private int populationProvide;

    public int getPopulationProvide() {
        return populationProvide;
    }

    public void setPopulationProvide(int value) {
        this.populationProvide = value;
    }
    private int populationUse;

    public int getPopulationUse() {
        return populationUse;
    }

    public void setPopulationUse(int value) {
        this.populationUse = value;
    }
    private int maxHealth;

    public int getMaxHealth() {
        return maxHealth;
    }

    public void setMaxHealth(int value) {
        this.maxHealth = value;
    }
    private int initialCost;

    public int getInitialCost() {
        return initialCost;
    }

    public void setInitialCost(int value) {
        this.initialCost = value;
    }
    private int sightRange;

    public int getSightRange() {
        return sightRange;
    }

    public void setSightRange(int value) {
        this.sightRange = value;
    }
    private int resourcePerHealth;

    public int getResourcePerHealth() {
        return resourcePerHealth;
    }

    public void setResourcePerHealth(int value) {
        this.resourcePerHealth = value;
    }
    private model.BuildProperties build;

    public model.BuildProperties getBuild() {
        return build;
    }

    public void setBuild(model.BuildProperties value) {
        this.build = value;
    }
    private model.AttackProperties attack;

    public model.AttackProperties getAttack() {
        return attack;
    }

    public void setAttack(model.AttackProperties value) {
        this.attack = value;
    }
    private model.RepairProperties repair;

    public model.RepairProperties getRepair() {
        return repair;
    }

    public void setRepair(model.RepairProperties value) {
        this.repair = value;
    }

    public EntityProperties(int size, int buildScore, int destroyScore, boolean canMove, int populationProvide, int populationUse, int maxHealth, int initialCost, int sightRange, int resourcePerHealth, model.BuildProperties build, model.AttackProperties attack, model.RepairProperties repair) {
        this.size = size;
        this.buildScore = buildScore;
        this.destroyScore = destroyScore;
        this.canMove = canMove;
        this.populationProvide = populationProvide;
        this.populationUse = populationUse;
        this.maxHealth = maxHealth;
        this.initialCost = initialCost;
        this.sightRange = sightRange;
        this.resourcePerHealth = resourcePerHealth;
        this.build = build;
        this.attack = attack;
        this.repair = repair;
    }

    public static EntityProperties readFrom(java.io.InputStream stream) throws java.io.IOException {
        int size;
        size = StreamUtil.readInt(stream);
        int buildScore;
        buildScore = StreamUtil.readInt(stream);
        int destroyScore;
        destroyScore = StreamUtil.readInt(stream);
        boolean canMove;
        canMove = StreamUtil.readBoolean(stream);
        int populationProvide;
        populationProvide = StreamUtil.readInt(stream);
        int populationUse;
        populationUse = StreamUtil.readInt(stream);
        int maxHealth;
        maxHealth = StreamUtil.readInt(stream);
        int initialCost;
        initialCost = StreamUtil.readInt(stream);
        int sightRange;
        sightRange = StreamUtil.readInt(stream);
        int resourcePerHealth;
        resourcePerHealth = StreamUtil.readInt(stream);
        model.BuildProperties build;
        if (StreamUtil.readBoolean(stream)) {
            build = model.BuildProperties.readFrom(stream);
        } else {
            build = null;
        }
        model.AttackProperties attack;
        if (StreamUtil.readBoolean(stream)) {
            attack = model.AttackProperties.readFrom(stream);
        } else {
            attack = null;
        }
        model.RepairProperties repair;
        if (StreamUtil.readBoolean(stream)) {
            repair = model.RepairProperties.readFrom(stream);
        } else {
            repair = null;
        }
        return new EntityProperties(size, buildScore, destroyScore, canMove, populationProvide, populationUse, maxHealth, initialCost, sightRange, resourcePerHealth, build, attack, repair);
    }

    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, size);
        StreamUtil.writeInt(stream, buildScore);
        StreamUtil.writeInt(stream, destroyScore);
        StreamUtil.writeBoolean(stream, canMove);
        StreamUtil.writeInt(stream, populationProvide);
        StreamUtil.writeInt(stream, populationUse);
        StreamUtil.writeInt(stream, maxHealth);
        StreamUtil.writeInt(stream, initialCost);
        StreamUtil.writeInt(stream, sightRange);
        StreamUtil.writeInt(stream, resourcePerHealth);
        if (build == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            build.writeTo(stream);
        }
        if (attack == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            attack.writeTo(stream);
        }
        if (repair == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            repair.writeTo(stream);
        }
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("EntityProperties { ");
        stringBuilder.append("size: ");
        stringBuilder.append(String.valueOf(size));
        stringBuilder.append(", ");
        stringBuilder.append("buildScore: ");
        stringBuilder.append(String.valueOf(buildScore));
        stringBuilder.append(", ");
        stringBuilder.append("destroyScore: ");
        stringBuilder.append(String.valueOf(destroyScore));
        stringBuilder.append(", ");
        stringBuilder.append("canMove: ");
        stringBuilder.append(String.valueOf(canMove));
        stringBuilder.append(", ");
        stringBuilder.append("populationProvide: ");
        stringBuilder.append(String.valueOf(populationProvide));
        stringBuilder.append(", ");
        stringBuilder.append("populationUse: ");
        stringBuilder.append(String.valueOf(populationUse));
        stringBuilder.append(", ");
        stringBuilder.append("maxHealth: ");
        stringBuilder.append(String.valueOf(maxHealth));
        stringBuilder.append(", ");
        stringBuilder.append("initialCost: ");
        stringBuilder.append(String.valueOf(initialCost));
        stringBuilder.append(", ");
        stringBuilder.append("sightRange: ");
        stringBuilder.append(String.valueOf(sightRange));
        stringBuilder.append(", ");
        stringBuilder.append("resourcePerHealth: ");
        stringBuilder.append(String.valueOf(resourcePerHealth));
        stringBuilder.append(", ");
        stringBuilder.append("build: ");
        stringBuilder.append(String.valueOf(build));
        stringBuilder.append(", ");
        stringBuilder.append("attack: ");
        stringBuilder.append(String.valueOf(attack));
        stringBuilder.append(", ");
        stringBuilder.append("repair: ");
        stringBuilder.append(String.valueOf(repair));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}