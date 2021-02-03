namespace TransGenTest.Model
{
    /// <summary>
    /// Player (strategy, client)
    /// </summary>
    public struct Player
    {
        /// <summary>
        /// Player's ID
        /// </summary>
        public int Id { get; set; }
        /// <summary>
        /// Current score
        /// </summary>
        public int Score { get; set; }
        /// <summary>
        /// Current amount of resource
        /// </summary>
        public int Resource { get; set; }
    
        public Player(int id, int score, int resource)
        {
            this.Id = id;
            this.Score = score;
            this.Resource = resource;
        }
    
        /// <summary> Read Player from reader </summary>
        public static Player ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Player();
            result.Id = reader.ReadInt32();
            result.Score = reader.ReadInt32();
            result.Resource = reader.ReadInt32();
            return result;
        }
    
        /// <summary> Write Player to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(Id);
            writer.Write(Score);
            writer.Write(Resource);
        }
    
        /// <summary> Get string representation of Player </summary>
        public override string ToString() {
            string stringResult = "Player { ";
            stringResult += "Id: ";
            stringResult += Id.ToString();
            stringResult += ", ";
            stringResult += "Score: ";
            stringResult += Score.ToString();
            stringResult += ", ";
            stringResult += "Resource: ";
            stringResult += Resource.ToString();
            stringResult += " }";
            return stringResult;
        }
    }
}