package model

import "io"
import . "trans_gen_test/stream"

type Player struct {
    Id int32
    Score int32
    Resource int32
}

func NewPlayer(id int32, score int32, resource int32) Player {
    return Player {
        Id: id,
        Score: score,
        Resource: resource,
    }
}

func ReadPlayer(reader io.Reader) Player {
    var id int32
    id = ReadInt32(reader)
    var score int32
    score = ReadInt32(reader)
    var resource int32
    resource = ReadInt32(reader)
    return Player {
        Id: id,
        Score: score,
        Resource: resource,
    }
}

func (player Player) Write(writer io.Writer) {
    id := player.Id
    WriteInt32(writer, id)
    score := player.Score
    WriteInt32(writer, score)
    resource := player.Resource
    WriteInt32(writer, resource)
}