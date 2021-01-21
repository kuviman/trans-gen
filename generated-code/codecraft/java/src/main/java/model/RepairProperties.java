package model;

import util.StreamUtil;

public class RepairProperties {
    private model.EntityType[] validTargets;

    public model.EntityType[] getValidTargets() {
        return validTargets;
    }

    public void setValidTargets(model.EntityType[] value) {
        this.validTargets = value;
    }
    private int power;

    public int getPower() {
        return power;
    }

    public void setPower(int value) {
        this.power = value;
    }

    public RepairProperties(model.EntityType[] validTargets, int power) {
        this.validTargets = validTargets;
        this.power = power;
    }

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

    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, validTargets.length);
        for (model.EntityType validTargetsElement : validTargets) {
            StreamUtil.writeInt(stream, validTargetsElement.tag);
        }
        StreamUtil.writeInt(stream, power);
    }
}