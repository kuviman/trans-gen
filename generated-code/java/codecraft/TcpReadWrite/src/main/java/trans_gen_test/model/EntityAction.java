package trans_gen_test.model;

import trans_gen_test.util.StreamUtil;

/**
 * Entity's action
 */
public class EntityAction {
    /**
     * Move action
     */
    private trans_gen_test.model.MoveAction moveAction;

    /**
     * Move action
     */
    public trans_gen_test.model.MoveAction getMoveAction() {
        return moveAction;
    }

    /**
     * Move action
     */
    public void setMoveAction(trans_gen_test.model.MoveAction value) {
        this.moveAction = value;
    }
    /**
     * Build action
     */
    private trans_gen_test.model.BuildAction buildAction;

    /**
     * Build action
     */
    public trans_gen_test.model.BuildAction getBuildAction() {
        return buildAction;
    }

    /**
     * Build action
     */
    public void setBuildAction(trans_gen_test.model.BuildAction value) {
        this.buildAction = value;
    }
    /**
     * Attack action
     */
    private trans_gen_test.model.AttackAction attackAction;

    /**
     * Attack action
     */
    public trans_gen_test.model.AttackAction getAttackAction() {
        return attackAction;
    }

    /**
     * Attack action
     */
    public void setAttackAction(trans_gen_test.model.AttackAction value) {
        this.attackAction = value;
    }
    /**
     * Repair action
     */
    private trans_gen_test.model.RepairAction repairAction;

    /**
     * Repair action
     */
    public trans_gen_test.model.RepairAction getRepairAction() {
        return repairAction;
    }

    /**
     * Repair action
     */
    public void setRepairAction(trans_gen_test.model.RepairAction value) {
        this.repairAction = value;
    }

    public EntityAction(trans_gen_test.model.MoveAction moveAction, trans_gen_test.model.BuildAction buildAction, trans_gen_test.model.AttackAction attackAction, trans_gen_test.model.RepairAction repairAction) {
        this.moveAction = moveAction;
        this.buildAction = buildAction;
        this.attackAction = attackAction;
        this.repairAction = repairAction;
    }

    /**
     * Read EntityAction from input stream
     */
    public static EntityAction readFrom(java.io.InputStream stream) throws java.io.IOException {
        trans_gen_test.model.MoveAction moveAction;
        if (StreamUtil.readBoolean(stream)) {
            moveAction = trans_gen_test.model.MoveAction.readFrom(stream);
        } else {
            moveAction = null;
        }
        trans_gen_test.model.BuildAction buildAction;
        if (StreamUtil.readBoolean(stream)) {
            buildAction = trans_gen_test.model.BuildAction.readFrom(stream);
        } else {
            buildAction = null;
        }
        trans_gen_test.model.AttackAction attackAction;
        if (StreamUtil.readBoolean(stream)) {
            attackAction = trans_gen_test.model.AttackAction.readFrom(stream);
        } else {
            attackAction = null;
        }
        trans_gen_test.model.RepairAction repairAction;
        if (StreamUtil.readBoolean(stream)) {
            repairAction = trans_gen_test.model.RepairAction.readFrom(stream);
        } else {
            repairAction = null;
        }
        return new EntityAction(moveAction, buildAction, attackAction, repairAction);
    }

    /**
     * Write EntityAction to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        if (moveAction == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            moveAction.writeTo(stream);
        }
        if (buildAction == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            buildAction.writeTo(stream);
        }
        if (attackAction == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            attackAction.writeTo(stream);
        }
        if (repairAction == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            repairAction.writeTo(stream);
        }
    }

    /**
     * Get string representation of EntityAction
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("EntityAction { ");
        stringBuilder.append("moveAction: ");
        stringBuilder.append(String.valueOf(moveAction));
        stringBuilder.append(", ");
        stringBuilder.append("buildAction: ");
        stringBuilder.append(String.valueOf(buildAction));
        stringBuilder.append(", ");
        stringBuilder.append("attackAction: ");
        stringBuilder.append(String.valueOf(attackAction));
        stringBuilder.append(", ");
        stringBuilder.append("repairAction: ");
        stringBuilder.append(String.valueOf(repairAction));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}