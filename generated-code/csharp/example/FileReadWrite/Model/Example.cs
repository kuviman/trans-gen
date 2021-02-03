namespace TransGenTest.Model
{
    /// <summary>
    /// Example
    /// </summary>
    public struct Example
    {
        /// <summary>
        /// OneOf
        /// </summary>
        public Model.OneOf OneOf { get; set; }
        /// <summary>
        /// Dictionary
        /// </summary>
        public System.Collections.Generic.IDictionary<Model.Enumeration, int> HashMap { get; set; }
        /// <summary>
        /// Optional int
        /// </summary>
        public int? OptionalInt { get; set; }
        /// <summary>
        /// Optional boolean
        /// </summary>
        public bool? OptionalBool { get; set; }
        /// <summary>
        /// Optional OneOf
        /// </summary>
        public Model.OneOf OptionalOneOf { get; set; }
        /// <summary>
        /// Optional struct
        /// </summary>
        public Model.Structure? OptionalStruct { get; set; }
        /// <summary>
        /// Optional enum
        /// </summary>
        public Model.Enumeration? OptionalEnum { get; set; }
    
        public Example(Model.OneOf oneOf, System.Collections.Generic.IDictionary<Model.Enumeration, int> hashMap, int? optionalInt, bool? optionalBool, Model.OneOf optionalOneOf, Model.Structure? optionalStruct, Model.Enumeration? optionalEnum)
        {
            this.OneOf = oneOf;
            this.HashMap = hashMap;
            this.OptionalInt = optionalInt;
            this.OptionalBool = optionalBool;
            this.OptionalOneOf = optionalOneOf;
            this.OptionalStruct = optionalStruct;
            this.OptionalEnum = optionalEnum;
        }
    
        /// <summary> Read Example from reader </summary>
        public static Example ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Example();
            result.OneOf = Model.OneOf.ReadFrom(reader);
            int hashMapSize = reader.ReadInt32();
            result.HashMap = new System.Collections.Generic.Dictionary<Model.Enumeration, int>(hashMapSize);
            for (int hashMapIndex = 0; hashMapIndex < hashMapSize; hashMapIndex++)
            {
                Model.Enumeration hashMapKey;
                int hashMapValue;
                hashMapKey = EnumerationHelper.ReadFrom(reader);
                hashMapValue = reader.ReadInt32();
                result.HashMap.Add(hashMapKey, hashMapValue);
            }
            if (reader.ReadBoolean())
            {
                result.OptionalInt = reader.ReadInt32();
            } else
            {
                result.OptionalInt = null;
            }
            if (reader.ReadBoolean())
            {
                result.OptionalBool = reader.ReadBoolean();
            } else
            {
                result.OptionalBool = null;
            }
            if (reader.ReadBoolean())
            {
                result.OptionalOneOf = Model.OneOf.ReadFrom(reader);
            } else
            {
                result.OptionalOneOf = null;
            }
            if (reader.ReadBoolean())
            {
                result.OptionalStruct = Model.Structure.ReadFrom(reader);
            } else
            {
                result.OptionalStruct = null;
            }
            if (reader.ReadBoolean())
            {
                result.OptionalEnum = EnumerationHelper.ReadFrom(reader);
            } else
            {
                result.OptionalEnum = null;
            }
            return result;
        }
    
        /// <summary> Write Example to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            OneOf.WriteTo(writer);
            writer.Write(HashMap.Count);
            foreach (var hashMapEntry in HashMap)
            {
                var hashMapKey = hashMapEntry.Key;
                var hashMapValue = hashMapEntry.Value;
                writer.Write((int) (hashMapKey));
                writer.Write(hashMapValue);
            }
            if (!OptionalInt.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                writer.Write(OptionalInt.Value);
            }
            if (!OptionalBool.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                writer.Write(OptionalBool.Value);
            }
            if (OptionalOneOf == null)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                OptionalOneOf.WriteTo(writer);
            }
            if (!OptionalStruct.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                OptionalStruct.Value.WriteTo(writer);
            }
            if (!OptionalEnum.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                writer.Write((int) (OptionalEnum.Value));
            }
        }
    
        /// <summary> Get string representation of Example </summary>
        public override string ToString() {
            string stringResult = "Example { ";
            stringResult += "OneOf: ";
            stringResult += OneOf.ToString();
            stringResult += ", ";
            stringResult += "HashMap: ";
            stringResult += "{ ";
            int hashMapIndex = 0;
            foreach (var hashMapEntry in HashMap)
            {
                if (hashMapIndex != 0) {
                    stringResult += ", ";
                }
                var hashMapKey = hashMapEntry.Key;
                stringResult += hashMapKey.ToString();
                stringResult += ": ";
                var hashMapValue = hashMapEntry.Value;
                stringResult += hashMapValue.ToString();
                hashMapIndex++;
            }
            stringResult += " }";
            stringResult += ", ";
            stringResult += "OptionalInt: ";
            if (!OptionalInt.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += OptionalInt.Value.ToString();
            }
            stringResult += ", ";
            stringResult += "OptionalBool: ";
            if (!OptionalBool.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += OptionalBool.Value.ToString();
            }
            stringResult += ", ";
            stringResult += "OptionalOneOf: ";
            if (OptionalOneOf == null)
            {
                stringResult += "null";
            } else
            {
                stringResult += OptionalOneOf.ToString();
            }
            stringResult += ", ";
            stringResult += "OptionalStruct: ";
            if (!OptionalStruct.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += OptionalStruct.Value.ToString();
            }
            stringResult += ", ";
            stringResult += "OptionalEnum: ";
            if (!OptionalEnum.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += OptionalEnum.Value.ToString();
            }
            stringResult += " }";
            return stringResult;
        }
    }
}