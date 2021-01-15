namespace TransGenTest.Model
{
    public struct Structure
    {
        public Model.OneOf OneOfOne { get; set; }
        public Model.OneOf OneOfTwo { get; set; }
        public System.Collections.Generic.IDictionary<Model.Enumeration, int> HashMap { get; set; }
        public string Text { get; set; }
        public double RealNumber { get; set; }
        public Structure(Model.OneOf oneOfOne, Model.OneOf oneOfTwo, System.Collections.Generic.IDictionary<Model.Enumeration, int> hashMap, string text, double realNumber)
        {
            this.OneOfOne = oneOfOne;
            this.OneOfTwo = oneOfTwo;
            this.HashMap = hashMap;
            this.Text = text;
            this.RealNumber = realNumber;
        }
        public static Structure ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Structure();
            result.OneOfOne = Model.OneOf.ReadFrom(reader);
            result.OneOfTwo = Model.OneOf.ReadFrom(reader);
            int HashMapSize = reader.ReadInt32();
            result.HashMap = new System.Collections.Generic.Dictionary<Model.Enumeration, int>(HashMapSize);
            for (int i = 0; i < HashMapSize; i++)
            {
                Model.Enumeration HashMapKey;
                switch (reader.ReadInt32())
                {
                case 0:
                    HashMapKey = Model.Enumeration.ValueOne;
                    break;
                case 1:
                    HashMapKey = Model.Enumeration.ValueTwo;
                    break;
                default:
                    throw new System.Exception("Unexpected tag value");
                }
                int HashMapValue;
                HashMapValue = reader.ReadInt32();
                result.HashMap.Add(HashMapKey, HashMapValue);
            }
            result.Text = System.Text.Encoding.UTF8.GetString(reader.ReadBytes(reader.ReadInt32()));
            result.RealNumber = reader.ReadDouble();
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            OneOfOne.WriteTo(writer);
            OneOfTwo.WriteTo(writer);
            writer.Write(HashMap.Count);
            foreach (var HashMapEntry in HashMap)
            {
                var HashMapKey = HashMapEntry.Key;
                var HashMapValue = HashMapEntry.Value;
                writer.Write((int) (HashMapKey));
                writer.Write(HashMapValue);
            }
            var TextData = System.Text.Encoding.UTF8.GetBytes(Text);
            writer.Write(TextData.Length);
            writer.Write(TextData);
            writer.Write(RealNumber);
        }
    }
}
