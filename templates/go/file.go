package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
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

func (c Case) String() string {
	return ""
}

type Input struct {
	t     int
	cases []Case
}

func parseInput(lines []string) Input {
	r := Input{}
	r.t, _ = strconv.Atoi(lines[0])
	return r
}

func (c Case) solve() string {
	return fmt.Sprintf("%s", c)
}

func main() {
	in := parseInput(readInput())
	for i, c := range in.cases {
		logCase(i+1, c.solve())
	}
}
