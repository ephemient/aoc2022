package com.github.ephemient.aoc2022

class Day5(lines: List<String>) {
    private val initialStacks: Map<Char, String>
    private val moves: List<Triple<Int, Char, Char>>
    init {
        val n = lines.indexOf("")
        initialStacks = buildMap {
            lines[n - 1].forEachIndexed { i, c ->
                if (!c.isDigit()) return@forEachIndexed
                for (j in 0 until n) {
                    if (lines[j].getOrNull(i)?.isWhitespace() == false) {
                        put(c, buildString(n - 1 - j) { for (k in j until n - 1) append(lines[k][i]) })
                        break
                    }
                }
            }
        }
        moves = buildList(lines.size - n - 1) {
            for (i in n + 1 until lines.size) {
                val (num, x, y) = PATTERN.matchEntire(lines[i])!!.destructured
                add(Triple(num.toInt(), x.single(), y.single()))
            }
        }
    }

    fun part1(): String = solve(true)

    fun part2(): String = solve(false)

    private fun solve(reverse: Boolean): String {
        val stacks = initialStacks.toMutableMap()
        for ((num, x, y) in moves) {
            val source = stacks.getValue(x)
            stacks[x] = source.drop(num)
            stacks[y] = (if (reverse) source.take(num).reversed() else source.take(num)) + stacks.getValue(y)
        }
        return stacks.values.joinToString("") { it[0].toString() }
    }

    companion object {
        private val PATTERN = """move (\d+) from (\d) to (\d)""".toRegex()
    }
}
