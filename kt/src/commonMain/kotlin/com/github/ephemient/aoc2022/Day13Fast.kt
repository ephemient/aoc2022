package com.github.ephemient.aoc2022

@Day
class Day13Fast(private val lines: List<String>) {
    @Day.Part
    fun part1(): Int =
        lines.chunked(3) { (a, b) -> a isInOrderRelativeTo b }
            .withIndex()
            .filter { it.value }
            .sumOf { it.index + 1 }

    @Day.Part
    fun part2(): Int {
        var x = 1
        var y = 1
        for (line in lines) {
            when {
                line.isEmpty() -> continue
                line isInOrderRelativeTo a -> x++
                line isInOrderRelativeTo b -> y++
            }
        }
        return x * (x + y)
    }

    private sealed class Token {
        object Open : Token()
        object Close : Token()
        data class Int(val value: kotlin.Int) : Token()
    }

    companion object {
        private const val a = "[[2]]"
        private const val b = "[[6]]"

        private fun CharSequence.tokenize(): Iterator<Token> = iterator {
            var index = 0
            while (index < length) {
                when (get(index)) {
                    '[' -> yield(Token.Open)
                    ']' -> yield(Token.Close)
                    in '0'..'9' -> {
                        val startIndex = index++
                        while (get(index) in '0'..'9') index++
                        yield(Token.Int(substring(startIndex, index).toInt()))
                        continue
                    }
                }
                index++
            }
        }

        @Suppress("CyclomaticComplexMethod", "NestedBlockDepth", "ReturnCount")
        private infix fun String.isInOrderRelativeTo(other: String): Boolean {
            val i = this.tokenize()
            val j = other.tokenize()
            while (i.hasNext() && j.hasNext()) {
                val x = i.next()
                val y = j.next()
                when {
                    x == Token.Close && y != Token.Close -> return true
                    x != Token.Close && y == Token.Close -> return false
                    x is Token.Int && y is Token.Int -> {
                        val comparison = x.value compareTo y.value
                        if (comparison != 0) return comparison < 0
                    }
                    x is Token.Int -> {
                        var z: Token
                        var depth = 0
                        do {
                            z = j.next()
                            depth++
                        } while (z == Token.Open)
                        if (z is Token.Int) {
                            val comparison = x.value compareTo z.value
                            if (comparison != 0) return comparison < 0
                            repeat(depth) {
                                if (j.next() != Token.Close) return true
                            }
                        } else {
                            return false
                        }
                    }
                    y is Token.Int -> {
                        var z: Token
                        var depth = 0
                        do {
                            z = i.next()
                            depth++
                        } while (z == Token.Open)
                        if (z is Token.Int) {
                            val comparison = z.value compareTo y.value
                            if (comparison != 0) return comparison < 0
                            repeat(depth) {
                                if (i.next() != Token.Close) return false
                            }
                        } else {
                            return true
                        }
                    }
                    else -> check(x == y)
                }
            }
            return i.hasNext() <= j.hasNext()
        }
    }
}
