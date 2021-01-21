namespace TransGenTest.Model
{
    public struct Example
    {
        public Model.OneOf OneOf { get; set; }
        public System.Collections.Generic.IDictionary<Model.Enumeration, int> HashMap { get; set; }
        public int? OptionalInt { get; set; }
        public bool? OptionalBool { get; set; }
        public Model.OneOf OptionalOneOf { get; set; }
        public Model.Structure? OptionalStruct { get; set; }
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
    }
}