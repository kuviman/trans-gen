package main

import (
	"bufio"
	. "trans_gen_test/codegame"
	"os"
	"fmt"
	"strconv"
)

func main() {
	inputFilePath := os.Args[1]
	outputFilePath := os.Args[2]
	repeat, err := strconv.Atoi(os.Args[3])
	if err != nil {
		panic(err)
	}

	for i := 0; i < repeat; i++ {
		inputFile, err := os.Open(inputFilePath)
		if err != nil {
			panic(err)
		}
		reader := bufio.NewReader(inputFile)
		input := ReadMessageGameModel(reader)

		if repeat == 1 {
			fmt.Println(input)
		}

		outputFile, err := os.Create(outputFilePath)
		if err != nil {
			panic(err)
		}
		writer := bufio.NewWriter(outputFile)

		input.Write(bufio.NewWriter(writer))
		err = writer.Flush()
		if err != nil {
			panic(err)
		}
	}
}