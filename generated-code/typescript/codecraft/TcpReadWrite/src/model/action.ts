import { EntityAction } from "./entity-action";
import { Stream } from "../stream";

/**
 * Player's action
 */
export class Action {
    /**
     * New actions for entities. If entity does not get new action, if will continue to perform previously set one
     */
    entityActions: Map<number, EntityAction>

    constructor(entityActions: Map<number, EntityAction>) {
        this.entityActions = entityActions;
    }

    /**
     * Read Action from input stream
     */
    static async readFrom(stream: Stream): Promise<Action> {
        let entityActions;
        entityActions = new Map();
        for (let entityActionsCount = await stream.readInt(); entityActionsCount > 0; entityActionsCount--) {
            let entityActionsKey;
            let entityActionsValue;
            entityActionsKey = await stream.readInt();
            entityActionsValue = await EntityAction.readFrom(stream);
            entityActions.set(entityActionsKey, entityActionsValue)
        }
        return new Action(entityActions)
    }

    /**
     * Write Action to output stream
     */
    async writeTo(stream: Stream) {
        let entityActions = this.entityActions;
        await stream.writeInt(entityActions.size);
        for (let [entityActionsKey, entityActionsValue] of entityActions) {
            await stream.writeInt(entityActionsKey);
            await entityActionsValue.writeTo(stream);
        }
    }
}