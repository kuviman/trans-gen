package trans_gen_test.model;

import trans_gen_test.util.StreamUtil;

/**
 * Auto attack options
 */
public class AutoAttack {
    /**
     * Maximum distance to pathfind
     */
    private int pathfindRange;

    /**
     * Maximum distance to pathfind
     */
    public int getPathfindRange() {
        return pathfindRange;
    }

    /**
     * Maximum distance to pathfind
     */
    public void setPathfindRange(int value) {
        this.pathfindRange = value;
    }
    /**
     * List of target entity types to try to attack. If empty, all types but resource are considered
     */
    private trans_gen_test.model.EntityType[] validTargets;

    /**
     * List of target entity types to try to attack. If empty, all types but resource are considered
     */
    public trans_gen_test.model.EntityType[] getValidTargets() {
        return validTargets;
    }

    /**
     * List of target entity types to try to attack. If empty, all types but resource are considered
     */
    public void setValidTargets(trans_gen_test.model.EntityType[] value) {
        this.validTargets = value;
    }

    public AutoAttack(int pathfindRange, trans_gen_test.model.EntityType[] validTargets) {
        this.pathfindRange = pathfindRange;
        this.validTargets = validTargets;
    }

    /**
     * Read AutoAttack from input stream
     */
    public static AutoAttack readFrom(java.io.InputStream stream) throws java.io.IOException {
        int pathfindRange;
        pathfindRange = StreamUtil.readInt(stream);
        trans_gen_test.model.EntityType[] validTargets;
        validTargets = new trans_gen_test.model.EntityType[StreamUtil.readInt(stream)];
        for (int validTargetsIndex = 0; validTargetsIndex < validTargets.length; validTargetsIndex++) {
            trans_gen_test.model.EntityType validTargetsElement;
            validTargetsElement = trans_gen_test.model.EntityType.readFrom(stream);
            validTargets[validTargetsIndex] = validTargetsElement;
        }
        return new AutoAttack(pathfindRange, validTargets);
    }

    /**
     * Write AutoAttack to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, pathfindRange);
        StreamUtil.writeInt(stream, validTargets.length);
        for (trans_gen_test.model.EntityType validTargetsElement : validTargets) {
            StreamUtil.writeInt(stream, validTargetsElement.tag);
        }
    }

    /**
     * Get string representation of AutoAttack
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("AutoAttack { ");
        stringBuilder.append("pathfindRange: ");
        stringBuilder.append(String.valueOf(pathfindRange));
        stringBuilder.append(", ");
        stringBuilder.append("validTargets: ");
        stringBuilder.append("[ ");
        for (int validTargetsIndex = 0; validTargetsIndex < validTargets.length; validTargetsIndex++) {
            if (validTargetsIndex != 0) {
                stringBuilder.append(", ");
            }
            trans_gen_test.model.EntityType validTargetsElement = validTargets[validTargetsIndex];
            stringBuilder.append(String.valueOf(validTargetsElement));
        }
        stringBuilder.append(" ]");
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}