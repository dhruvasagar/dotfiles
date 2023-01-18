package main

import (
	"bufio"
	"fmt"
	"os"
	"time"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func part1(_lines []string) int {
	return 0
}

func part2(_lines []string) int {
	return 0
}

func main() {
	s := time.Now()
	lines := readInput()
	s1 := time.Now()
	fmt.Println(part1(lines))
	e1 := time.Since(s1)
	s2 := time.Now()
	fmt.Println(part2(lines))
	e2 := time.Since(s2)
	e := time.Since(s)
	fmt.Printf("Time for part1: %s, part2: %s, total: %s\n", e1, e2, e)
}
