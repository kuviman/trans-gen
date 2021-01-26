package model

import "io"
import . "trans_gen_test/stream"

// Example enumeration
type Enumeration int32

const (
    // First option
    EnumerationValueOne Enumeration = 0
    // Second option
    EnumerationValueTwo Enumeration = 1
)

// Read Enumeration from reader
func ReadEnumeration(reader io.Reader) Enumeration {
    switch ReadInt32(reader) {
    case 0:
        return EnumerationValueOne
    case 1:
        return EnumerationValueTwo
    }
    panic("Unexpected tag value")
}

// Get string representation of Enumeration
func EnumerationToString(enumeration Enumeration) string {
    switch enumeration {
    case EnumerationValueOne:
        return "ValueOne"
    case EnumerationValueTwo:
        return "ValueTwo"
    }
    panic("Impossible happened")
}