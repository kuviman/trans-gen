package model;

import util.StreamUtil;

/**
 * Entity's repair properties
 */
public class RepairProperties {
    /**
     * Valid target entity types
     */
    private model.EntityType[] validTargets;

    /**
     * Valid target entity types
     */
    public model.EntityType[] getValidTargets() {
        return validTargets;
    }

    /**
     * Valid target entity types
     */
    public void setValidTargets(model.EntityType[] value) {
        this.validTargets = value;
    }
    /**
     * Health restored in one tick
     */
    private int power;

    /**
     * Health restored in one tick
     */
    public int getPower() {
        return power;
    }

    /**
     * Health restored in one tick
     */
    public void setPower(int value) {
        this.power = value;
    }

    public RepairProperties(model.EntityType[] validTargets, int power) {
        this.validTargets = validTargets;
        this.power = power;
    }

    /**
     * Read RepairProperties from input stream
     */
    public static RepairProperties readFrom(java.io.InputStream stream) throws java.io.IOException {
        model.EntityType[] validTargets;
        validTargets = new model.EntityType[StreamUtil.readInt(stream)];
        for (int validTargetsIndex = 0; validTargetsIndex < validTargets.length; validTargetsIndex++) {
            model.EntityType validTargetsElement;
            validTargetsElement = model.EntityType.readFrom(stream);
            validTargets[validTargetsIndex] = validTargetsElement;
        }
        int power;
        power = StreamUtil.readInt(stream);
        return new RepairProperties(validTargets, power);
    }

    /**
     * Write RepairProperties to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, validTargets.length);
        for (model.EntityType validTargetsElement : validTargets) {
            StreamUtil.writeInt(stream, validTargetsElement.tag);
        }
        StreamUtil.writeInt(stream, power);
    }

    /**
     * Get string representation of RepairProperties
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("RepairProperties { ");
        stringBuilder.append("validTargets: ");
        stringBuilder.append("[ ");
        for (int validTargetsIndex = 0; validTargetsIndex < validTargets.length; validTargetsIndex++) {
            if (validTargetsIndex != 0) {
                stringBuilder.append(", ");
            }
            model.EntityType validTargetsElement = validTargets[validTargetsIndex];
            stringBuilder.append(String.valueOf(validTargetsElement));
        }
        stringBuilder.append(" ]");
        stringBuilder.append(", ");
        stringBuilder.append("power: ");
        stringBuilder.append(String.valueOf(power));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}