using System;
using System.IO;
using System.Net.Sockets;

namespace TransGenTest
{
    public class Runner
    {
        public static void Main(string[] args)
        {
            string host = args[0];
            int port = int.Parse(args[1]);
            bool stdout = bool.Parse(args[2]);

            using (var tcpClient = new TcpClient(host, port))
            {
                using (var stream = new BufferedStream(tcpClient.GetStream()))
                {
                    var reader = new BinaryReader(stream);
                    var writer = new BinaryWriter(stream);
                    while (reader.ReadBoolean())
                    {
                        Example input = Example.ReadFrom(reader);
                        if (stdout)
                        {
                            Console.WriteLine(input.ToString());
                        }
                        input.WriteTo(writer);
                        writer.Flush();
                    }
                }
            }
        }
    }
}