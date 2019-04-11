package main

import (
	"bufio"
	"fmt"
	"os"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func logCase(num int, out string) {
	fmt.Printf("Case #%d: %s\n", num, out)
}

type Case struct {
}

type Input struct {
	t     int
	cases []Case
}

func parseInput(lines []string) Input {
	r := Input{}
	return r
}

func solve(c Case) string {
	return ""
}

func main() {
	in := parseInput(readInput())
	for i, c := range in.cases {
		logCase(i+1, solve(c))
	}
}
