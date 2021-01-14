using System;
using System.IO;

namespace Aicup2020CodecraftModel
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

            Model.PlayerView input;
            using (var stream = new FileStream(inputFile, FileMode.Open))
            {
                using (var reader = new BinaryReader(stream))
                {
                    input = Model.PlayerView.ReadFrom(reader);
                }
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