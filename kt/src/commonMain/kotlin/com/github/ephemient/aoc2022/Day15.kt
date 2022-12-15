package com.github.ephemient.aoc2022

import kotlin.math.abs

@Day
class Day15(lines: List<String>) {
    private val inputs = lines.map {
        val x0 = it.substring(it.indexOf("x=") + 2, it.indexOf(',')).toInt()
        val y0 = it.substring(it.indexOf("y=") + 2, it.indexOf(':')).toInt()
        val x1 = it.substring(it.lastIndexOf("x=") + 2, it.lastIndexOf(',')).toInt()
        val y1 = it.substring(it.lastIndexOf("y=") + 2).toInt()
        x0 to y0 to (x1 to y1)
    }

    @Day.Part
    fun part1(y: Int = 2000000): Int {
        val intervals = mutableListOf<IntRange>()
        val beacons = mutableSetOf<Int>()
        for ((sensor, beacon) in inputs) {
            val dx = abs(sensor.first - beacon.first) + abs(sensor.second - beacon.second) - abs(y - sensor.second)
            if (dx >= 0) intervals.addInterval(sensor.first - dx..sensor.first + dx)
            if (beacon.second == y) beacons.add(beacon.first)
        }
        return intervals.sumOf { it.last - it.first + 1 } - beacons.size
    }

    @Day.Part
    fun part2(size: Int = 4000000): Long = sequence {
        for (y in 0..size) {
            val intervals = mutableListOf<IntRange>()
            for ((sensor, beacon) in inputs) {
                val dx = abs(sensor.first - beacon.first) + abs(sensor.second - beacon.second) - abs(y - sensor.second)
                val lo = (sensor.first - dx).coerceAtLeast(0)
                val hi = (sensor.first + dx).coerceAtMost(size)
                if (lo <= hi) intervals.addInterval(lo..hi)
            }
            val hi = intervals.fold(0) { prev, interval ->
                for (x in prev until interval.first) yield(4000000L * x + y)
                interval.last + 1
            }
            for (x in hi..size) yield(4000000L * x + y)
        }
    }.single()
}

internal fun MutableList<IntRange>.addInterval(range: IntRange) {
    val loIndex = binarySearch { it.last.compareTo(range.first - 1) }.let { it shr 31 xor it }
    val hiIndex = binarySearch(fromIndex = loIndex) { it.first.compareTo(range.last + 1) }.let { it shr 31 xor it }
    val mergedRange = if (loIndex < hiIndex) {
        minOf(this[loIndex].first, range.first)..maxOf(this[hiIndex - 1].last, range.last)
    } else {
        range
    }
    subList(loIndex, hiIndex).clear()
    add(loIndex, mergedRange)
}
