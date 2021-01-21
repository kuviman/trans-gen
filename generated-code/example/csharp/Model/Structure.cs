namespace TransGenTest.Model
{
    public struct Structure
    {
        public Model.OneOf OneOfOne { get; set; }
        public Model.OneOf OneOfTwo { get; set; }
        public System.Collections.Generic.IDictionary<Model.Enumeration, int> HashMap { get; set; }
        public string Text { get; set; }
        public float FloatNumber { get; set; }
        public double DoubleNumber { get; set; }
    
        public Structure(Model.OneOf oneOfOne, Model.OneOf oneOfTwo, System.Collections.Generic.IDictionary<Model.Enumeration, int> hashMap, string text, float floatNumber, double doubleNumber)
        {
            this.OneOfOne = oneOfOne;
            this.OneOfTwo = oneOfTwo;
            this.HashMap = hashMap;
            this.Text = text;
            this.FloatNumber = floatNumber;
            this.DoubleNumber = doubleNumber;
        }
    
        public static Structure ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Structure();
            result.OneOfOne = Model.OneOf.ReadFrom(reader);
            result.OneOfTwo = Model.OneOf.ReadFrom(reader);
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
            result.Text = System.Text.Encoding.UTF8.GetString(reader.ReadBytes(reader.ReadInt32()));
            result.FloatNumber = reader.ReadSingle();
            result.DoubleNumber = reader.ReadDouble();
            return result;
        }
    
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            OneOfOne.WriteTo(writer);
            OneOfTwo.WriteTo(writer);
            writer.Write(HashMap.Count);
            foreach (var hashMapEntry in HashMap)
            {
                var hashMapKey = hashMapEntry.Key;
                var hashMapValue = hashMapEntry.Value;
                writer.Write((int) (hashMapKey));
                writer.Write(hashMapValue);
            }
            var textData = System.Text.Encoding.UTF8.GetBytes(Text);
            writer.Write(textData.Length);
            writer.Write(textData);
            writer.Write(FloatNumber);
            writer.Write(DoubleNumber);
        }
    }
}