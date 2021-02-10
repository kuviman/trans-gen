namespace TransGenTest.Model
{
    /// <summary>
    /// Player's action
    /// </summary>
    public struct Action
    {
        /// <summary>
        /// New actions for entities. If entity does not get new action, if will continue to perform previously set one
        /// </summary>
        public System.Collections.Generic.IDictionary<int, Model.EntityAction> EntityActions { get; set; }
    
        public Action(System.Collections.Generic.IDictionary<int, Model.EntityAction> entityActions)
        {
            this.EntityActions = entityActions;
        }
    
        /// <summary> Read Action from reader </summary>
        public static Action ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Action();
            int entityActionsSize = reader.ReadInt32();
            result.EntityActions = new System.Collections.Generic.Dictionary<int, Model.EntityAction>(entityActionsSize);
            for (int entityActionsIndex = 0; entityActionsIndex < entityActionsSize; entityActionsIndex++)
            {
                int entityActionsKey;
                Model.EntityAction entityActionsValue;
                entityActionsKey = reader.ReadInt32();
                entityActionsValue = Model.EntityAction.ReadFrom(reader);
                result.EntityActions.Add(entityActionsKey, entityActionsValue);
            }
            return result;
        }
    
        /// <summary> Write Action to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(EntityActions.Count);
            foreach (var entityActionsEntry in EntityActions)
            {
                var entityActionsKey = entityActionsEntry.Key;
                var entityActionsValue = entityActionsEntry.Value;
                writer.Write(entityActionsKey);
                entityActionsValue.WriteTo(writer);
            }
        }
    
        /// <summary> Get string representation of Action </summary>
        public override string ToString() {
            string stringResult = "Action { ";
            stringResult += "EntityActions: ";
            stringResult += "{ ";
            int entityActionsIndex = 0;
            foreach (var entityActionsEntry in EntityActions)
            {
                if (entityActionsIndex != 0) {
                    stringResult += ", ";
                }
                var entityActionsKey = entityActionsEntry.Key;
                stringResult += entityActionsKey.ToString();
                stringResult += ": ";
                var entityActionsValue = entityActionsEntry.Value;
                stringResult += entityActionsValue.ToString();
                entityActionsIndex++;
            }
            stringResult += " }";
            stringResult += " }";
            return stringResult;
        }
    }
}