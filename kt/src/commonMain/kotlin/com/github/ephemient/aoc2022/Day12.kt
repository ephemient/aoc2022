package com.github.ephemient.aoc2022

@Day
class Day12(private val lines: List<String>) {
    @Day.Part
    fun part1(): Int? {
        val start = lines.withIndex().firstNotNullOf { (y, line) ->
            line.withIndex().find { (_, c) -> c == 'S' }?.let { (x, _) -> IntPair(y, x) }
        }
        return bfs(start) { (y, x) ->
            val a = lines[y][x]
            arrayOf(IntPair(y - 1, x), IntPair(y, x - 1), IntPair(y, x + 1), IntPair(y + 1, x)).filter { (y, x) ->
                val b = lines.getOrNull(y)?.getOrNull(x) ?: return@filter false
                if (a == 'S') b == 'a' else if (b == 'E') a == 'z' else b - a <= 1
            }
        }.firstOrNull { (_, value) -> lines[value.first][value.second] == 'E' }?.index
    }

    @Day.Part
    fun part2(): Int? {
        val start = lines.withIndex().firstNotNullOf { (y, line) ->
            line.withIndex().find { (_, c) -> c == 'E' }?.let { (x, _) -> IntPair(y, x) }
        }
        return bfs(start) { (y, x) ->
            val a = lines[y][x]
            arrayOf(IntPair(y - 1, x), IntPair(y, x - 1), IntPair(y, x + 1), IntPair(y + 1, x)).filter { (y, x) ->
                val b = lines.getOrNull(y)?.getOrNull(x) ?: return@filter false
                if (a == 'E') b == 'z' else a - b <= 1
            }
        }.firstOrNull { (_, value) ->
            lines[value.first][value.second] == 'a'
        }?.index
    }
}

private fun <T> bfs(start: T, next: (T) -> Iterable<T>): Sequence<IndexedValue<T>> = sequence {
    val seen = mutableSetOf(start)
    val queue = ArrayDeque(listOf(IndexedValue(0, start)))
    while (queue.isNotEmpty()) {
        val a = queue.removeFirst()
        yield(a)
        for (b in next(a.value)) {
            if (seen.add(b)) {
                queue.add(IndexedValue(a.index + 1, b))
            }
        }
    }
}
