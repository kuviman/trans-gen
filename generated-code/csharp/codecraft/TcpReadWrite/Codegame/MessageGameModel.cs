namespace TransGenTest.Codegame
{
    /// <summary>
    /// Client or server message
    /// </summary>
    public abstract class MessageGameModel
    {
        /// <summary> Write MessageGameModel to writer </summary>
        public abstract void WriteTo(System.IO.BinaryWriter writer);

        /// <summary> Read MessageGameModel from reader </summary>
        public static MessageGameModel ReadFrom(System.IO.BinaryReader reader)
        {
            switch (reader.ReadInt32())
            {
                case Client.TAG:
                    return Client.ReadFrom(reader);
                case Server.TAG:
                    return Server.ReadFrom(reader);
                default:
                    throw new System.Exception("Unexpected tag value");
            }
        }

        /// <summary>
        /// Client message
        /// </summary>
        public class Client : MessageGameModel
        {
            public const int TAG = 0;
        
            /// <summary>
            /// Message
            /// </summary>
            public Codegame.ClientMessage Message { get; set; }
        
            public Client() { }
        
            public Client(Codegame.ClientMessage message)
            {
                this.Message = message;
            }
        
            /// <summary> Read Client from reader </summary>
            public static new Client ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new Client();
                result.Message = Codegame.ClientMessage.ReadFrom(reader);
                return result;
            }
        
            /// <summary> Write Client to writer </summary>
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                Message.WriteTo(writer);
            }
        
            /// <summary> Get string representation of Client </summary>
            public override string ToString() {
                string stringResult = "Client { ";
                stringResult += "Message: ";
                stringResult += Message.ToString();
                stringResult += " }";
                return stringResult;
            }
        }

        /// <summary>
        /// Server message
        /// </summary>
        public class Server : MessageGameModel
        {
            public const int TAG = 1;
        
            /// <summary>
            /// Message
            /// </summary>
            public Codegame.ServerMessage Message { get; set; }
        
            public Server() { }
        
            public Server(Codegame.ServerMessage message)
            {
                this.Message = message;
            }
        
            /// <summary> Read Server from reader </summary>
            public static new Server ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new Server();
                result.Message = Codegame.ServerMessage.ReadFrom(reader);
                return result;
            }
        
            /// <summary> Write Server to writer </summary>
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                Message.WriteTo(writer);
            }
        
            /// <summary> Get string representation of Server </summary>
            public override string ToString() {
                string stringResult = "Server { ";
                stringResult += "Message: ";
                stringResult += Message.ToString();
                stringResult += " }";
                return stringResult;
            }
        }
    }
}