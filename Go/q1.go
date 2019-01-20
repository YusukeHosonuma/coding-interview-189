package main

import "fmt"

func main() {
	s := "こんにちはこ"
	if IsUnique(s) {
		fmt.Println("ユニーク文字列です。")
	} else {
		fmt.Println("ユニーク文字列じゃないよ！")
	}
}

// IsUnique is return string is unique or not
func IsUnique(s string) bool {
	counts := make(map[rune]int)
	runes := []rune(s)

	for _, value := range runes {
		counts[value]++
		if counts[value] > 1 {
			return false
		}
	}
	return true
}
