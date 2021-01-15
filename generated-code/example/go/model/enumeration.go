package model

import "io"
import . "trans_gen_test/stream"

type Enumeration int32
const (
    EnumerationValueOne Enumeration = 0
    EnumerationValueTwo Enumeration = 1
)
func ReadEnumeration(reader io.Reader) Enumeration {
    switch ReadInt32(reader) {
    case 0:
        return EnumerationValueOne
    case 1:
        return EnumerationValueTwo
    }
    panic("Unexpected tag value")
}
