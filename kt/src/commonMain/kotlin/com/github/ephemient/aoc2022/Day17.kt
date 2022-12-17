package com.github.ephemient.aoc2022

@Day
class Day17(lines: List<String>) {
    private val jet = lines.single()

    @Day.Part
    fun part1(): Long = solve(2022)

    @Day.Part
    fun part2(): Long = solve(1000000000000)

    private fun solve(n: Long): Long {
        val buffer = mutableListOf<State>()
        val cycle = generateSequence(Triple(State(0, byteArrayOf(127)), 0, 0)) { (state, jetIndex, rockIndex) ->
            var i = jetIndex
            val rock = rocks[rockIndex]
            val maxX = 7 - rock.maxOf { it.length }
            var x = 2
            var y = state.height + 3
            while (true) {
                val x2 = when (jet[i]) {
                    '<' -> (x - 1).coerceAtLeast(0)
                    '>' -> (x + 1).coerceAtMost(maxX)
                    else -> throw IllegalArgumentException("${jet.take(i)}[${jet[i]}${jet.drop(i + 1)}")
                }
                i = (i + 1) % jet.length
                x = if (state.contains(x2, y, rock)) x else x2
                if (state.contains(x, y - 1, rock)) break
                y--
            }
            Triple(state.plus(x, y, rock), i, (rockIndex + 1) % rocks.size)
        }.onEach { buffer.add(it.first) }.map { (state, jetIndex, rockIndex) ->
            Triple(
                state.rows.asList(),
                jetIndex,
                rockIndex
            )
        }.take((n + 1).coerceAtMost(Int.MAX_VALUE.toLong()).toInt()).asIterable().findCycle()
        return if (cycle != null) {
            val height1 = buffer[cycle.first].height
            val height2 = buffer[cycle.second].height
            val dividend = n - cycle.first
            val divisor = cycle.second - cycle.first
            val quotient = dividend / divisor
            val remainder = dividend % divisor
            buffer[cycle.first + remainder.toIntExact()].height + (height2 - height1) * quotient
        } else {
            buffer[n.toIntExact()].height.toLong()
        }
    }

    private class State(height: Int, val rows: ByteArray) {
        private val yOffset = height - rows.size
        val height: Int get() = yOffset + rows.size

        @Suppress("ReturnCount")
        fun contains(x: Int, y: Int, rock: Array<String>): Boolean {
            require(y >= yOffset) { "$yOffset/$y/$height" }
            rock.forEachIndexed { dy, line ->
                val row = rows.getOrElse(y - yOffset + dy) { return false }
                line.forEachIndexed { dx, c -> if (c == '#' && row.toInt() and 1.shl(x + dx) != 0) return true }
            }
            return false
        }

        @Suppress("CyclomaticComplexMethod")
        fun plus(x: Int, y: Int, rock: Array<String>): State {
            require(y >= yOffset)
            val newHeight = maxOf(height, y + rock.size)
            val rows = rows.copyOf(newHeight - yOffset)
            rock.forEachIndexed { dy, line ->
                var row = rows[y - yOffset + dy]
                line.forEachIndexed { dx, c -> if (c == '#') row = (row.toInt() or 1.shl(x + dx)).toByte() }
                rows[y - yOffset + dy] = row
            }
            val visible = ByteArray(rows.size + 1).apply { set(lastIndex, 1) }
            val queue = mutableListOf(visible.lastIndex shl 3)
            while (queue.isNotEmpty()) {
                val pos = queue.removeLast()
                if (pos < visible.lastIndex shl 3 && rows[pos shr 3].toInt() and 1.shl(pos and 7) != 0) continue
                if (pos > 7 && visible[pos - 8 shr 3].toInt() and 1.shl(pos and 7) == 0) {
                    visible[pos - 8 shr 3] = (visible[pos - 8 shr 3].toInt() or 1.shl(pos and 7)).toByte()
                    queue.add(pos - 8)
                }
                if (pos and 7 != 0 && visible[pos shr 3].toInt() and 1.shl(pos - 1 and 7) == 0) {
                    visible[pos shr 3] = (visible[pos shr 3].toInt() or 1.shl(pos - 1 and 7)).toByte()
                    queue.add(pos - 1)
                }
                if (pos and 7 < 6 && visible[pos shr 3].toInt() and 1.shl(pos + 1 and 7) == 0) {
                    visible[pos shr 3] = (visible[pos shr 3].toInt() or 1.shl(pos + 1 and 7)).toByte()
                    queue.add(pos + 1)
                }
                if (pos < visible.lastIndex shl 3 && visible[pos + 8 shr 3].toInt() and 1.shl(pos and 7) == 0) {
                    visible[pos + 8 shr 3] = (visible[pos + 8 shr 3].toInt() or 1.shl(pos and 7)).toByte()
                    queue.add(pos + 8)
                }
            }
            rows.forEachIndexed { i, row -> rows[i] = (row.toInt() and visible[i].toInt()).toByte() }
            val trim = rows.indexOfFirst { it != 0.toByte() }
            return State(newHeight, if (trim > 0) rows.copyOfRange(fromIndex = trim, toIndex = rows.size) else rows)
        }
    }
}

private val rocks = arrayOf(
    arrayOf("####"),
    arrayOf(".#.", "###", ".#."),
    arrayOf("###", "..#", "..#"),
    arrayOf("#", "#", "#", "#"),
    arrayOf("##", "##")
)

private fun <T> Iterable<T>.findCycle(): IntPair? {
    val indices = mutableMapOf<T, Int>()
    forEachIndexed { j, value ->
        val i = indices.getOrPut(value) { j }
        if (i < j) return i to j
    }
    return null
}

private fun Long.toIntExact(): Int {
    require(this in Int.MIN_VALUE.toLong()..Int.MAX_VALUE.toLong())
    return this.toInt()
}
