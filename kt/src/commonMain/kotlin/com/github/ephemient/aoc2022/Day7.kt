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

@Day
class Day7a(lines: List<String>) {
    private val sizes = buildMap {
        put(emptyList(), 0)
        var cwd = emptyList<String>()
        for (line in lines) {
            val match = PATTERN.matchEntire(line) ?: continue
            match.groups[1]?.value?.let { dir ->
                cwd = when (dir) {
                    "/" -> emptyList()
                    ".." -> cwd.dropLast(1)
                    else -> cwd + dir
                }
            } ?: match.groups[2]?.value?.toIntOrNull()?.let { size ->
                var dir = cwd
                while (true) {
                    put(dir, getOrElse(dir) { 0 } + size)
                    if (dir.isEmpty()) break
                    dir = dir.subList(0, dir.lastIndex)
                }
            }
        }
    }

    @Day.Part
    fun part1(): Int = sizes.values.sumOf { if (it <= 100000) it else 0 }

    @Day.Part
    fun part2(): Int {
        val total = sizes.getValue(emptyList())
        return sizes.values.sorted().first { 70000000 - (total - it) >= 30000000 }
    }
}

@Day
class Day7b(lines: List<String>) {
    private val sizes = buildMap {
        put("", 0)
        var cwd = ""
        for (line in lines) {
            if (line.startsWith("$ cd ")) {
                val dir = line.substring(5)
                cwd = when (dir) {
                    "/" -> ""
                    ".." -> cwd.substringBeforeLast('/', "")
                    else -> if (cwd.isEmpty()) dir else "$cwd/$dir"
                }
            } else {
                val digits = line.indexOfFirst { !it.isDigit() }
                if (digits > 0) {
                    val size = line.substring(0, digits).toInt()
                    var dir = cwd
                    while (true) {
                        put(dir, getOrElse(dir) { 0 } + size)
                        if (dir.isEmpty()) break
                        dir = dir.substringBeforeLast('/', "")
                    }
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

@Day
class Day7c(lines: List<String>) {
    private val sizes = IntCounter<String>().apply {
        add("", 0)
        var cwd = ""
        for (line in lines) {
            if (line.startsWith("$ cd ")) {
                val dir = line.substring(5)
                cwd = when (dir) {
                    "/" -> ""
                    ".." -> cwd.substringBeforeLast('/', "")
                    else -> if (cwd.isEmpty()) dir else "$cwd/$dir"
                }
            } else {
                val digits = line.indexOfFirst { !it.isDigit() }
                if (digits > 0) {
                    val size = line.substring(0, digits).toInt()
                    var dir = cwd
                    while (true) {
                        add(dir, size)
                        if (dir.isEmpty()) break
                        dir = dir.substringBeforeLast('/', "")
                    }
                }
            }
        }
    }

    @Day.Part
    fun part1(): Int = sizes.values.sumOf { if (it <= 100000) it else 0 }

    @Day.Part
    fun part2(): Int {
        val total = sizes[""]
        return sizes.values.sorted().first { 70000000 - (total - it) >= 30000000 }
    }

    @Day.Part
    fun part3(): Int {
        val goal = sizes[""] - 40000000
        val sizes = sizes.values.toIntArray()
        sizes.sort()
        var lo = 0
        var hi = sizes.size
        while (hi - lo > 1) {
            val mid = lo + (hi - lo) / 2
            if (sizes[mid - 1] < goal) lo = mid else hi = mid
        }
        return sizes[lo]
    }

    @Day.Part
    fun part4(): Int {
        val total = sizes[""]
        return sizes.values.asSequence().filter { 70000000 - (total - it) >= 30000000 }.min()
    }

    @Day.Part
    fun part5(): Int {
        val total = sizes[""]
        return sizes.values.fold(total) { min, size -> if (size in total - 40000000 until min) size else min }
    }
}
