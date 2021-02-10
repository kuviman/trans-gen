using System;
using System.IO;

namespace TransGenTest
{
    public class Runner
    {
        public static void Main(string[] args)
        {
            string inputFile = args[0];
            string outputFile = args[1];
            int repeat = int.Parse(args[2]);

            for (int i = 0; i < repeat; i++)
            {
                Example input;
                using (var stream = new FileStream(inputFile, FileMode.Open))
                {
                    using (var reader = new BinaryReader(stream))
                    {
                        input = Example.ReadFrom(reader);
                    }
                }
                if (repeat == 1)
                {
                    Console.WriteLine(input.ToString());
                }
                using (var stream = new FileStream(outputFile, FileMode.Create))
                {
                    using (var writer = new BinaryWriter(stream)) {
                        input.WriteTo(writer);
                    }
                }
            }
        }
    }
}