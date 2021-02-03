namespace TransGenTest

open System
open System.IO
open System.Net.Sockets

module Runner =

    [<EntryPoint>]
    let main argv =
        if argv.Length <> 2 then
            failwith "Pass host and port as parameters"
        let host = argv.[0]
        let port = argv.[1] |> Int32.Parse

        use client = new TcpClient(host, port)
        let stream = new BufferedStream(client.GetStream())

        let reader = new BinaryReader(stream)
        let input = Model.PlayerView.readFrom(reader)

        Console.WriteLine(input)

        let writer = new BinaryWriter(stream)
        input.writeTo(writer)
        writer.Flush()

        0