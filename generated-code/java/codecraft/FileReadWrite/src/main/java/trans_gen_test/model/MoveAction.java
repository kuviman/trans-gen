package trans_gen_test.model;

import trans_gen_test.util.StreamUtil;

/**
 * Move action
 */
public class MoveAction {
    /**
     * Target position
     */
    private trans_gen_test.Vec2Int target;

    /**
     * Target position
     */
    public trans_gen_test.Vec2Int getTarget() {
        return target;
    }

    /**
     * Target position
     */
    public void setTarget(trans_gen_test.Vec2Int value) {
        this.target = value;
    }
    /**
     * Whether to try find closest position, if path to target is not found
     */
    private boolean findClosestPosition;

    /**
     * Whether to try find closest position, if path to target is not found
     */
    public boolean isFindClosestPosition() {
        return findClosestPosition;
    }

    /**
     * Whether to try find closest position, if path to target is not found
     */
    public void setFindClosestPosition(boolean value) {
        this.findClosestPosition = value;
    }
    /**
     * Whether to destroy other entities on the way
     */
    private boolean breakThrough;

    /**
     * Whether to destroy other entities on the way
     */
    public boolean isBreakThrough() {
        return breakThrough;
    }

    /**
     * Whether to destroy other entities on the way
     */
    public void setBreakThrough(boolean value) {
        this.breakThrough = value;
    }

    public MoveAction(trans_gen_test.Vec2Int target, boolean findClosestPosition, boolean breakThrough) {
        this.target = target;
        this.findClosestPosition = findClosestPosition;
        this.breakThrough = breakThrough;
    }

    /**
     * Read MoveAction from input stream
     */
    public static MoveAction readFrom(java.io.InputStream stream) throws java.io.IOException {
        trans_gen_test.Vec2Int target;
        target = trans_gen_test.Vec2Int.readFrom(stream);
        boolean findClosestPosition;
        findClosestPosition = StreamUtil.readBoolean(stream);
        boolean breakThrough;
        breakThrough = StreamUtil.readBoolean(stream);
        return new MoveAction(target, findClosestPosition, breakThrough);
    }

    /**
     * Write MoveAction to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        target.writeTo(stream);
        StreamUtil.writeBoolean(stream, findClosestPosition);
        StreamUtil.writeBoolean(stream, breakThrough);
    }

    /**
     * Get string representation of MoveAction
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("MoveAction { ");
        stringBuilder.append("target: ");
        stringBuilder.append(String.valueOf(target));
        stringBuilder.append(", ");
        stringBuilder.append("findClosestPosition: ");
        stringBuilder.append(String.valueOf(findClosestPosition));
        stringBuilder.append(", ");
        stringBuilder.append("breakThrough: ");
        stringBuilder.append(String.valueOf(breakThrough));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}