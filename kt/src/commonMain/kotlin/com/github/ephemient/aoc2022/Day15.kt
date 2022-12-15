package com.github.ephemient.aoc2022

import kotlin.math.abs

@Day
@Suppress("ComplexCondition")
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

    @Day.Part
    fun part2a(size: Int = 4000000): Long {
        val acc = QuadAccumulator(-size..size, 0..2 * size)
        for ((sensor, beacon) in inputs) {
            val d = abs(sensor.first - beacon.first) + abs(sensor.second - beacon.second)
            val u = sensor.second - sensor.first
            val v = sensor.second + sensor.first
            acc.add(u - d..u + d, v - d..v + d)
        }
        return acc.holes().flatMap { (uRange, vRange) ->
            if (
                vRange.last - uRange.first < 0 || vRange.first - uRange.last > 2 * size ||
                vRange.last + uRange.last < 0 || vRange.first + uRange.first > 2 * size
            ) {
                emptySequence()
            } else {
                sequence {
                    for (u in uRange) {
                        for (v in vRange) {
                            if (u xor v and 1 != 0) continue
                            val x = (v - u) / 2
                            val y = (v + u) / 2
                            if (x in 0..size && y in 0..size) yield(4000000L * x + y)
                        }
                    }
                }
            }
        }.single()
    }

    private sealed class Quad {
        object Solid : Quad()
        data class Split(val a: Quad?, val b: Quad?, val c: Quad?, val d: Quad?) : Quad()
    }

    private class QuadAccumulator(private val uRange: IntRange, private val vRange: IntRange) {
        private val size = 1 shl 32 - (uRange.last - uRange.first).countLeadingZeroBits()
        private var quad: Quad? = null

        private fun Quad?.add(quRange: IntRange, qvRange: IntRange, uRange: IntRange, vRange: IntRange): Quad? =
            if (uRange.isEmpty() || vRange.isEmpty() || this == Quad.Solid) {
                this
            } else {
                this as Quad.Split? // compiler isn't smart enough
                if (
                    uRange.first <= quRange.first && uRange.last >= quRange.last &&
                    vRange.first <= qvRange.first && vRange.last >= qvRange.last
                ) {
                    Quad.Solid
                } else {
                    val uMid = quRange.first + (quRange.last - quRange.first + 1) / 2
                    val vMid = qvRange.first + (qvRange.last - qvRange.first + 1) / 2
                    val quRange0 = quRange.first until uMid
                    val quRange1 = uMid..quRange.last
                    val qvRange0 = qvRange.first until vMid
                    val qvRange1 = vMid..qvRange.last
                    val uRange0 = uRange.first..minOf(uRange.last, uMid - 1)
                    val uRange1 = maxOf(uRange.first, uMid)..uRange.last
                    val vRange0 = vRange.first..minOf(vRange.last, vMid - 1)
                    val vRange1 = maxOf(vRange.first, vMid)..vRange.last
                    val a = this?.a.add(quRange0, qvRange0, uRange0, vRange0)
                    val b = this?.b.add(quRange0, qvRange1, uRange0, vRange1)
                    val c = this?.c.add(quRange1, qvRange0, uRange1, vRange0)
                    val d = this?.d.add(quRange1, qvRange1, uRange1, vRange1)
                    if (a == Quad.Solid && b == Quad.Solid && c == Quad.Solid && d == Quad.Solid) {
                        Quad.Solid
                    } else {
                        Quad.Split(a, b, c, d)
                    }
                }
            }

        fun add(uRange: IntRange, vRange: IntRange) {
            quad = quad.add(
                this.uRange.first until this.uRange.first + size,
                this.vRange.first until this.vRange.first + size,
                uRange,
                vRange,
            )
        }

        private fun Quad?.holes(uRange: IntRange, vRange: IntRange): Sequence<Pair<IntRange, IntRange>> = when (this) {
            null -> {
                val maxU = this@QuadAccumulator.uRange.last
                val maxV = this@QuadAccumulator.vRange.last
                if (uRange.first <= maxU && vRange.first <= maxV) {
                    sequenceOf(uRange.first..minOf(uRange.last, maxU) to vRange.first..minOf(vRange.last, maxV))
                } else {
                    emptySequence()
                }
            }
            Quad.Solid -> sequenceOf()
            is Quad.Split -> sequence {
                val uMid = uRange.first + (uRange.last - uRange.first + 1) / 2
                val vMid = vRange.first + (vRange.last - vRange.first + 1) / 2
                yieldAll(a.holes(uRange.first until uMid, vRange.first until vMid))
                yieldAll(b.holes(uRange.first until uMid, vMid..vRange.last))
                yieldAll(c.holes(uMid..uRange.last, vRange.first until vMid))
                yieldAll(d.holes(uMid..uRange.last, vMid..vRange.last))
            }
        }

        fun holes(): Sequence<Pair<IntRange, IntRange>> =
            quad.holes(uRange.first until uRange.first + size, vRange.first until vRange.first + size)
    }
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
