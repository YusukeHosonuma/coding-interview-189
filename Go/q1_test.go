package main

import "testing"

func TestIsUnique(t *testing.T) {
	var tests = []struct {
		input    string
		expected bool
	}{
		{"", true},
		{"A", true},
		{"AB", true},
		{"AA", false},
		{"Hello", false},
		{"こんにちは", true},
	}
	for _, test := range tests {
		if got := IsUnique(test.input); got != test.expected {
			t.Errorf(`IsUnique(%q) = %v`, test.input, got)
		}
	}
}
