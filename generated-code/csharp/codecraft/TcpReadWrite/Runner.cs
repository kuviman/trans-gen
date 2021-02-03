using System;
using System.IO;
using System.Net.Sockets;

namespace TransGenTest
{
    public class Runner
    {
        public static void Main(string[] args)
        {
            if (args.Length != 2)
            {
                throw new Exception("Pass host and port as parameters");
            }
            string host = args[0];
            int port = int.Parse(args[1]);

            using (var tcpClient = new TcpClient(host, port))
            {
                using (var stream = new BufferedStream(tcpClient.GetStream()))
                {
                    var reader = new BinaryReader(stream);
                    Model.PlayerView input = Model.PlayerView.ReadFrom(reader);
                    Console.WriteLine(input.ToString());
                    var writer = new BinaryWriter(stream);
                    input.WriteTo(writer);
                    writer.Flush();
                }
            }
        }
    }
}