package main

import (
	"bufio"
	. "trans_gen_test/model"
	"os"
	"fmt"
	"net"
	"strconv"
)

func main() {
	if len(os.Args) != 3 {
		panic("Pass host and port as parameters")
	}
	host := os.Args[1]
	portInt, err := strconv.Atoi(os.Args[2])
	port := uint16(portInt)
	if err != nil {
		panic(err)
	}

	conn, err := net.Dial("tcp", host+":"+strconv.Itoa(int(port)))
	if err != nil {
		panic(err)
	}

	reader := bufio.NewReader(conn)
	input := ReadExample(reader)

	fmt.Println(input)

	writer := bufio.NewWriter(conn)
	input.Write(writer)
	err = writer.Flush()
	if err != nil {
		panic(err)
	}

	err = conn.Close()
	if err != nil {
		panic(err)
	}
}