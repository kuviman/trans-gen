package main

import (
	"bufio"
	. "codecraft/model"
	"os"
)

func main() {
	if len(os.Args) != 3 {
		panic("Pass input and output as parameters")
	}
	inputFilePath := os.Args[1]
	outputFilePath := os.Args[2]

	inputFile, err := os.Open(inputFilePath)
	if err != nil {
		panic(err)
	}
	reader := bufio.NewReader(inputFile)
	input := ReadPlayerView(reader)

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
