package com.github.ephemient.aoc2022

@Day
class Day7(lines: List<String>) {
    private val sizes = buildMap {
        put("", 0)
        var cwd = ""
        for (line in lines) {
            val match = PATTERN.matchEntire(line) ?: continue
            match.groups[1]?.value?.let { dir ->
                cwd = when (dir) {
                    "/" -> ""
                    ".." -> cwd.substringBeforeLast('/', "")
                    else -> if (cwd.isEmpty()) dir else "$cwd/$dir"
                }
            } ?: match.groups[2]?.value?.toIntOrNull()?.let { size ->
                var dir = cwd
                while (true) {
                    put(dir, getOrElse(dir) { 0 } + size)
                    if (dir.isEmpty()) break
                    dir = dir.substringBeforeLast('/', "")
                }
            }
        }
    }

    @Day.Part
    fun part1(): Int = sizes.values.sumOf { if (it <= 100000) it else 0 }

    @Day.Part
    fun part2(): Int {
        val total = sizes.getValue("")
        return sizes.values.sorted().first { 70000000 - (total - it) >= 30000000 }
    }
}

private val PATTERN = """[$] cd (.*)|(\d+).*""".toRegex()
