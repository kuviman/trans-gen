namespace TransGenTest.Model
{
    /// <summary>
    /// Entity's action
    /// </summary>
    public struct EntityAction
    {
        /// <summary>
        /// Move action
        /// </summary>
        public Model.MoveAction? MoveAction { get; set; }
        /// <summary>
        /// Build action
        /// </summary>
        public Model.BuildAction? BuildAction { get; set; }
        /// <summary>
        /// Attack action
        /// </summary>
        public Model.AttackAction? AttackAction { get; set; }
        /// <summary>
        /// Repair action
        /// </summary>
        public Model.RepairAction? RepairAction { get; set; }
    
        public EntityAction(Model.MoveAction? moveAction, Model.BuildAction? buildAction, Model.AttackAction? attackAction, Model.RepairAction? repairAction)
        {
            this.MoveAction = moveAction;
            this.BuildAction = buildAction;
            this.AttackAction = attackAction;
            this.RepairAction = repairAction;
        }
    
        /// <summary> Read EntityAction from reader </summary>
        public static EntityAction ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new EntityAction();
            if (reader.ReadBoolean())
            {
                result.MoveAction = Model.MoveAction.ReadFrom(reader);
            } else
            {
                result.MoveAction = null;
            }
            if (reader.ReadBoolean())
            {
                result.BuildAction = Model.BuildAction.ReadFrom(reader);
            } else
            {
                result.BuildAction = null;
            }
            if (reader.ReadBoolean())
            {
                result.AttackAction = Model.AttackAction.ReadFrom(reader);
            } else
            {
                result.AttackAction = null;
            }
            if (reader.ReadBoolean())
            {
                result.RepairAction = Model.RepairAction.ReadFrom(reader);
            } else
            {
                result.RepairAction = null;
            }
            return result;
        }
    
        /// <summary> Write EntityAction to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            if (!MoveAction.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                MoveAction.Value.WriteTo(writer);
            }
            if (!BuildAction.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                BuildAction.Value.WriteTo(writer);
            }
            if (!AttackAction.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                AttackAction.Value.WriteTo(writer);
            }
            if (!RepairAction.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                RepairAction.Value.WriteTo(writer);
            }
        }
    
        /// <summary> Get string representation of EntityAction </summary>
        public override string ToString() {
            string stringResult = "EntityAction { ";
            stringResult += "MoveAction: ";
            if (!MoveAction.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += MoveAction.Value.ToString();
            }
            stringResult += ", ";
            stringResult += "BuildAction: ";
            if (!BuildAction.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += BuildAction.Value.ToString();
            }
            stringResult += ", ";
            stringResult += "AttackAction: ";
            if (!AttackAction.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += AttackAction.Value.ToString();
            }
            stringResult += ", ";
            stringResult += "RepairAction: ";
            if (!RepairAction.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += RepairAction.Value.ToString();
            }
            stringResult += " }";
            return stringResult;
        }
    }
}