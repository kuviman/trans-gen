package trans_gen_test.model;

import trans_gen_test.util.StreamUtil;

/**
 * Player's action
 */
public class Action {
    /**
     * New actions for entities. If entity does not get new action, if will continue to perform previously set one
     */
    private java.util.Map<Integer, trans_gen_test.model.EntityAction> entityActions;

    /**
     * New actions for entities. If entity does not get new action, if will continue to perform previously set one
     */
    public java.util.Map<Integer, trans_gen_test.model.EntityAction> getEntityActions() {
        return entityActions;
    }

    /**
     * New actions for entities. If entity does not get new action, if will continue to perform previously set one
     */
    public void setEntityActions(java.util.Map<Integer, trans_gen_test.model.EntityAction> value) {
        this.entityActions = value;
    }

    public Action(java.util.Map<Integer, trans_gen_test.model.EntityAction> entityActions) {
        this.entityActions = entityActions;
    }

    /**
     * Read Action from input stream
     */
    public static Action readFrom(java.io.InputStream stream) throws java.io.IOException {
        java.util.Map<Integer, trans_gen_test.model.EntityAction> entityActions;
        int entityActionsSize = StreamUtil.readInt(stream);
        entityActions = new java.util.HashMap<>(entityActionsSize);
        for (int entityActionsIndex = 0; entityActionsIndex < entityActionsSize; entityActionsIndex++) {
            int entityActionsKey;
            entityActionsKey = StreamUtil.readInt(stream);
            trans_gen_test.model.EntityAction entityActionsValue;
            entityActionsValue = trans_gen_test.model.EntityAction.readFrom(stream);
            entityActions.put(entityActionsKey, entityActionsValue);
        }
        return new Action(entityActions);
    }

    /**
     * Write Action to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, entityActions.size());
        for (java.util.Map.Entry<Integer, trans_gen_test.model.EntityAction> entityActionsEntry : entityActions.entrySet()) {
            int entityActionsKey = entityActionsEntry.getKey();
            StreamUtil.writeInt(stream, entityActionsKey);
            trans_gen_test.model.EntityAction entityActionsValue = entityActionsEntry.getValue();
            entityActionsValue.writeTo(stream);
        }
    }

    /**
     * Get string representation of Action
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("Action { ");
        stringBuilder.append("entityActions: ");
        stringBuilder.append(String.valueOf(entityActions));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}