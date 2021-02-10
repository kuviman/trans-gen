package trans_gen_test.model;

import trans_gen_test.util.StreamUtil;

/**
 * Entity properties
 */
public class EntityProperties {
    /**
     * Size. Entity has a form of a square with side of this length
     */
    private int size;

    /**
     * Size. Entity has a form of a square with side of this length
     */
    public int getSize() {
        return size;
    }

    /**
     * Size. Entity has a form of a square with side of this length
     */
    public void setSize(int value) {
        this.size = value;
    }
    /**
     * Score for building this entity
     */
    private int buildScore;

    /**
     * Score for building this entity
     */
    public int getBuildScore() {
        return buildScore;
    }

    /**
     * Score for building this entity
     */
    public void setBuildScore(int value) {
        this.buildScore = value;
    }
    /**
     * Score for destroying this entity
     */
    private int destroyScore;

    /**
     * Score for destroying this entity
     */
    public int getDestroyScore() {
        return destroyScore;
    }

    /**
     * Score for destroying this entity
     */
    public void setDestroyScore(int value) {
        this.destroyScore = value;
    }
    /**
     * Whether this entity can move
     */
    private boolean canMove;

    /**
     * Whether this entity can move
     */
    public boolean isCanMove() {
        return canMove;
    }

    /**
     * Whether this entity can move
     */
    public void setCanMove(boolean value) {
        this.canMove = value;
    }
    /**
     * Number of population points this entity provides, if active
     */
    private int populationProvide;

    /**
     * Number of population points this entity provides, if active
     */
    public int getPopulationProvide() {
        return populationProvide;
    }

    /**
     * Number of population points this entity provides, if active
     */
    public void setPopulationProvide(int value) {
        this.populationProvide = value;
    }
    /**
     * Number of population points this entity uses
     */
    private int populationUse;

    /**
     * Number of population points this entity uses
     */
    public int getPopulationUse() {
        return populationUse;
    }

    /**
     * Number of population points this entity uses
     */
    public void setPopulationUse(int value) {
        this.populationUse = value;
    }
    /**
     * Maximum health points
     */
    private int maxHealth;

    /**
     * Maximum health points
     */
    public int getMaxHealth() {
        return maxHealth;
    }

    /**
     * Maximum health points
     */
    public void setMaxHealth(int value) {
        this.maxHealth = value;
    }
    /**
     * Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
     */
    private int initialCost;

    /**
     * Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
     */
    public int getInitialCost() {
        return initialCost;
    }

    /**
     * Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
     */
    public void setInitialCost(int value) {
        this.initialCost = value;
    }
    /**
     * If fog of war is enabled, maximum distance at which other entities are considered visible
     */
    private int sightRange;

    /**
     * If fog of war is enabled, maximum distance at which other entities are considered visible
     */
    public int getSightRange() {
        return sightRange;
    }

    /**
     * If fog of war is enabled, maximum distance at which other entities are considered visible
     */
    public void setSightRange(int value) {
        this.sightRange = value;
    }
    /**
     * Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
     */
    private int resourcePerHealth;

    /**
     * Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
     */
    public int getResourcePerHealth() {
        return resourcePerHealth;
    }

    /**
     * Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
     */
    public void setResourcePerHealth(int value) {
        this.resourcePerHealth = value;
    }
    /**
     * Build properties, if entity can build
     */
    private trans_gen_test.model.BuildProperties build;

    /**
     * Build properties, if entity can build
     */
    public trans_gen_test.model.BuildProperties getBuild() {
        return build;
    }

    /**
     * Build properties, if entity can build
     */
    public void setBuild(trans_gen_test.model.BuildProperties value) {
        this.build = value;
    }
    /**
     * Attack properties, if entity can attack
     */
    private trans_gen_test.model.AttackProperties attack;

    /**
     * Attack properties, if entity can attack
     */
    public trans_gen_test.model.AttackProperties getAttack() {
        return attack;
    }

    /**
     * Attack properties, if entity can attack
     */
    public void setAttack(trans_gen_test.model.AttackProperties value) {
        this.attack = value;
    }
    /**
     * Repair properties, if entity can repair
     */
    private trans_gen_test.model.RepairProperties repair;

    /**
     * Repair properties, if entity can repair
     */
    public trans_gen_test.model.RepairProperties getRepair() {
        return repair;
    }

    /**
     * Repair properties, if entity can repair
     */
    public void setRepair(trans_gen_test.model.RepairProperties value) {
        this.repair = value;
    }

    public EntityProperties(int size, int buildScore, int destroyScore, boolean canMove, int populationProvide, int populationUse, int maxHealth, int initialCost, int sightRange, int resourcePerHealth, trans_gen_test.model.BuildProperties build, trans_gen_test.model.AttackProperties attack, trans_gen_test.model.RepairProperties repair) {
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

    /**
     * Read EntityProperties from input stream
     */
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
        trans_gen_test.model.BuildProperties build;
        if (StreamUtil.readBoolean(stream)) {
            build = trans_gen_test.model.BuildProperties.readFrom(stream);
        } else {
            build = null;
        }
        trans_gen_test.model.AttackProperties attack;
        if (StreamUtil.readBoolean(stream)) {
            attack = trans_gen_test.model.AttackProperties.readFrom(stream);
        } else {
            attack = null;
        }
        trans_gen_test.model.RepairProperties repair;
        if (StreamUtil.readBoolean(stream)) {
            repair = trans_gen_test.model.RepairProperties.readFrom(stream);
        } else {
            repair = null;
        }
        return new EntityProperties(size, buildScore, destroyScore, canMove, populationProvide, populationUse, maxHealth, initialCost, sightRange, resourcePerHealth, build, attack, repair);
    }

    /**
     * Write EntityProperties to output stream
     */
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

    /**
     * Get string representation of EntityProperties
     */
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