using System;
using System.IO;

namespace TransGenTest
{
    public class Runner
    {
        public static void Main(string[] args)
        {
            if (args.Length != 2)
            {
                throw new Exception("Pass input and output as parameters");
            }
            string inputFile = args[0];
            string outputFile = args[1]; 

            Model.Example input;
            using (var stream = new FileStream(inputFile, FileMode.Open))
            {
                using (var reader = new BinaryReader(stream))
                {
                    input = Model.Example.ReadFrom(reader);
                }
            }
            Console.WriteLine(input.ToString());
            using (var stream = new FileStream(outputFile, FileMode.Create))
            {
                using (var writer = new BinaryWriter(stream)) {
                    input.WriteTo(writer);
                }
            }
        }
    }
}