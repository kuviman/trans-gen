package trans_gen_test.model;

import trans_gen_test.util.StreamUtil;

/**
 * Repair action
 */
public class RepairAction {
    /**
     * Target entity's ID
     */
    private int target;

    /**
     * Target entity's ID
     */
    public int getTarget() {
        return target;
    }

    /**
     * Target entity's ID
     */
    public void setTarget(int value) {
        this.target = value;
    }

    public RepairAction(int target) {
        this.target = target;
    }

    /**
     * Read RepairAction from input stream
     */
    public static RepairAction readFrom(java.io.InputStream stream) throws java.io.IOException {
        int target;
        target = StreamUtil.readInt(stream);
        return new RepairAction(target);
    }

    /**
     * Write RepairAction to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, target);
    }

    /**
     * Get string representation of RepairAction
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("RepairAction { ");
        stringBuilder.append("target: ");
        stringBuilder.append(String.valueOf(target));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}