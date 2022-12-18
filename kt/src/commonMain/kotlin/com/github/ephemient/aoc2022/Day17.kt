package com.github.ephemient.aoc2022

@Day
class Day17(lines: List<String>) {
    private val jet = lines.single()

    @Day.Part
    fun part1(): Long = solve(2022)

    @Day.Part
    fun part2(): Long = solve(1000000000000)

    private fun solve(n: Long): Long {
        var jetIndex = 0
        var state = State(0, byteArrayOf(127))
        val seen = mutableMapOf<Triple<Int, Int, List<Byte>>, Long>()
        val heights = mutableListOf<Long>()
        for (i in 0 until n) {
            val rockIndex = (i % rocks.size).toIntExact()
            seen.put(Triple(rockIndex, jetIndex, state.rows.asList()), i)?.let { j ->
                val q = (n - j) / (i - j)
                val r = (n - j) % (i - j)
                return heights[(j + r).toIntExact()] + q * (state.height - heights[j.toIntExact()])
            }
            val (width, rock) = rocks[rockIndex]
            var x = 2
            var y = state.rows.size + 3
            while (!state.contains(x, y, rock)) {
                val x2 = when (val c = jet[jetIndex]) {
                    '<' -> x - 1
                    '>' -> x + 1
                    else -> throw IllegalArgumentException("${jet.take(jetIndex)}$c${jet.drop(jetIndex + 1)}")
                }
                jetIndex = (jetIndex + 1) % jet.length
                if (x2 in 0..7 - width && !state.contains(x2, y, rock)) x = x2
                y--
            }
            heights.add(state.height)
            state = state.plus(x, y + 1, rock)
        }
        return state.height
    }

    private class State(val height: Long, val rows: ByteArray) {
        fun contains(x: Int, y: Int, rock: ByteArray): Boolean {
            require(y >= 0) { "$y/${rows.size}" }
            return rock.withIndex().any { (dy, row) ->
                rows.getOrElse(y + dy) { return false }.toInt() and row.toInt().shl(x) != 0
            }
        }

        @Suppress("CyclomaticComplexMethod")
        fun plus(x: Int, y: Int, rock: ByteArray): State {
            val rows = rows.copyOf(maxOf(rows.size, y + rock.size))
            val height = height - this.rows.size + rows.size
            rock.forEachIndexed { dy, row -> rows[y + dy] = (rows[y + dy].toInt() or row.toInt().shl(x)).toByte() }
            val visible = ByteArray(rows.size + 1).apply { set(lastIndex, 1) }
            val queue = mutableListOf(visible.lastIndex shl 3)
            while (queue.isNotEmpty()) {
                val pos = queue.removeLast()
                if (pos < visible.lastIndex shl 3 && rows[pos shr 3].toInt() and 1.shl(pos and 7) != 0) continue
                for (next in intArrayOf(pos - 8, pos - 1, pos + 1, pos + 8)) {
                    if (
                        next and 7 != 7 && next in 0 until visible.size.shl(3) &&
                        visible[next shr 3].toInt() and 1.shl(next and 7) == 0
                    ) {
                        visible[next shr 3] = (visible[next shr 3].toInt() or 1.shl(next and 7)).toByte()
                        queue.add(next)
                    }
                }
            }
            rows.forEachIndexed { i, row -> rows[i] = (row.toInt() and visible[i].toInt()).toByte() }
            val trim = rows.indexOfFirst { it != 0.toByte() }
            return State(height, if (trim > 0) rows.copyOfRange(fromIndex = trim, toIndex = rows.size) else rows)
        }
    }
}

private val rocks = arrayOf(
    IndexedValue(4, byteArrayOf(15)),
    IndexedValue(3, byteArrayOf(2, 7, 2)),
    IndexedValue(3, byteArrayOf(7, 4, 4)),
    IndexedValue(1, byteArrayOf(1, 1, 1, 1)),
    IndexedValue(2, byteArrayOf(3, 3)),
)

private fun Long.toIntExact(): Int {
    require(this in Int.MIN_VALUE.toLong()..Int.MAX_VALUE.toLong())
    return this.toInt()
}
