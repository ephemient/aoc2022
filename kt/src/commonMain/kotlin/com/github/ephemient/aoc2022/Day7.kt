package com.github.ephemient.aoc2022

@Day
class Day7(lines: List<String>) {
    private val total: Int
    private val sizes: Map<String, Int>
    init {
        var total = 0
        sizes = buildMap {
            var cwd = ""
            for (line in lines) {
                val match = PATTERN.matchEntire(line) ?: continue
                match.groups[1]?.value?.let { dir ->
                    cwd = when (dir) {
                        "/" -> "/"
                        ".." -> cwd.substringBeforeLast('/').ifEmpty { "/" }
                        else -> "$cwd/$dir"
                    }
                } ?: match.groups[2]?.value?.toIntOrNull()?.let { size ->
                    var dir = cwd
                    while (dir.isNotEmpty()) {
                        put(dir, getOrElse(dir) { 0 } + size)
                        dir = dir.substringBeforeLast('/')
                    }
                    total += size
                }
            }
        }
        this.total = total
    }

    @Day.Part
    fun part1(): Int = sizes.values.sumOf { if (it <= 100000) it else 0 }

    @Day.Part
    fun part2(): Int = sizes.values.sorted().firstOrNull() { 70000000 - (total - it) >= 30000000 } ?: total
}

private val PATTERN = """[$] cd (.*)|(\d+).*""".toRegex()
