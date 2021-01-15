namespace TransGenTest

open System
open System.IO

module Runner =

    [<EntryPoint>]
    let main argv =
        if argv.Length <> 2 then
            failwith "Pass input and output as parameters"
        let inputFile = argv.[0]
        let outputFile = argv.[1]

        use inputStream = new FileStream(inputFile, FileMode.Open)
        use reader = new BinaryReader(inputStream)
        let input = Model.Structure.readFrom(reader)

        use outputStream = new FileStream(outputFile, FileMode.Create)
        use writer = new BinaryWriter(outputStream)
        input.writeTo(writer)

        0
