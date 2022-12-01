package com.github.ephemient.aoc2022

class Day1(lines: List<String>) {
    private val sums = buildList {
        var sum = 0
        for (line in lines) {
            val value = line.toIntOrNull()
            if (value != null) {
                sum += value
            } else {
                add(sum)
                sum = 0
            }
        }
        add(sum)
    }

    fun part1(): Int = sums.max()

    fun part2(): Int {
        val sums = sums.toIntArray()
        sums.quickSelectDescending(2)
        return sums[0] + sums[1] + sums[2]
    }
}

private fun IntArray.quickSelectDescending(index: Int) {
    require(index in indices)
    var start = 0
    var end = size
    var iter = 0
    while (end - start > 1) {
        require(iter++ < 120)
        val pivotIndex = start + (end - start) / 2
        val pivot = this[pivotIndex]
        this[pivotIndex] = this[end - 1]
        var n = start
        for (i in start until end - 1) {
            val tmp = this[i]
            if (tmp < pivot) continue
            this[i] = this[n]
            this[n++] = tmp
        }
        this[end - 1] = this[n]
        this[n] = pivot
        if (n < index) {
            start = n + 1
        } else {
            end = n
        }
    }
}
