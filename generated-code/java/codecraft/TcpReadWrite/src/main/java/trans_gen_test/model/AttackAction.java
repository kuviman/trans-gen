package trans_gen_test.model;

import trans_gen_test.util.StreamUtil;

/**
 * Attack action
 */
public class AttackAction {
    /**
     * If specified, target entity's ID
     */
    private Integer target;

    /**
     * If specified, target entity's ID
     */
    public Integer getTarget() {
        return target;
    }

    /**
     * If specified, target entity's ID
     */
    public void setTarget(Integer value) {
        this.target = value;
    }
    /**
     * If specified, configures auto attacking
     */
    private trans_gen_test.model.AutoAttack autoAttack;

    /**
     * If specified, configures auto attacking
     */
    public trans_gen_test.model.AutoAttack getAutoAttack() {
        return autoAttack;
    }

    /**
     * If specified, configures auto attacking
     */
    public void setAutoAttack(trans_gen_test.model.AutoAttack value) {
        this.autoAttack = value;
    }

    public AttackAction(Integer target, trans_gen_test.model.AutoAttack autoAttack) {
        this.target = target;
        this.autoAttack = autoAttack;
    }

    /**
     * Read AttackAction from input stream
     */
    public static AttackAction readFrom(java.io.InputStream stream) throws java.io.IOException {
        Integer target;
        if (StreamUtil.readBoolean(stream)) {
            target = StreamUtil.readInt(stream);
        } else {
            target = null;
        }
        trans_gen_test.model.AutoAttack autoAttack;
        if (StreamUtil.readBoolean(stream)) {
            autoAttack = trans_gen_test.model.AutoAttack.readFrom(stream);
        } else {
            autoAttack = null;
        }
        return new AttackAction(target, autoAttack);
    }

    /**
     * Write AttackAction to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        if (target == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            StreamUtil.writeInt(stream, target);
        }
        if (autoAttack == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            autoAttack.writeTo(stream);
        }
    }

    /**
     * Get string representation of AttackAction
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("AttackAction { ");
        stringBuilder.append("target: ");
        stringBuilder.append(String.valueOf(target));
        stringBuilder.append(", ");
        stringBuilder.append("autoAttack: ");
        stringBuilder.append(String.valueOf(autoAttack));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}