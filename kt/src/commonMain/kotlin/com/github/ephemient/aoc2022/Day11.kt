package com.github.ephemient.aoc2022

@Day
class Day11(lines: List<String>) {
    private val monkeys = buildMap {
        val iterator = lines.iterator()
        while (iterator.hasNext()) {
            val line = iterator.next()
            if (line.isEmpty()) continue
            put(
                line.substringAfterLast(' ').substringBefore(':'),
                Monkey(
                    startingItems = iterator.next().substringAfter(": ").split(", ").map { it.toLong() },
                    operation = iterator.next().substringAfter(": ").toOperation(),
                    test = iterator.next().substringAfterLast(' ').toInt(),
                    ifTrue = iterator.next().substringAfterLast(' '),
                    ifFalse = iterator.next().substringAfterLast(' '),
                )
            )
        }
    }

    @Day.Part
    fun part1(): Long = solve(20) { it.floorDiv(3) }

    @Day.Part
    fun part2(): Long {
        val base = monkeys.values.fold(1L) { acc, monkey -> acc * monkey.test }
        return solve(10000) { it.mod(base) }
    }

    private fun solve(iterations: Int, post: (Long) -> Long): Long {
        val currentItems = monkeys.mapValues { it.value.startingItems.toMutableList() }
        val counts = LongArray(monkeys.size) { 0 }
        repeat(iterations) {
            monkeys.entries.forEachIndexed { i, (name, monkey) ->
                val items = with(currentItems.getValue(name)) { toList().also { clear() } }
                for (old in items) {
                    val new = post(monkey.operation(old))
                    val target = if (new % monkey.test == 0L) monkey.ifTrue else monkey.ifFalse
                    currentItems.getValue(target).add(new)
                }
                counts[i] += items.size.toLong()
            }
        }
        counts.sortDescending()
        return counts[0] * counts[1]
    }

    private data class Monkey(
        val startingItems: List<Long>,
        val operation: Operation,
        val test: Int,
        val ifTrue: String,
        val ifFalse: String,
    )

    private sealed class Operation {
        abstract operator fun invoke(value: Long): Long

        object Square : Operation() {
            override fun invoke(value: Long): Long = value * value
        }
        data class Times(val other: Int) : Operation() {
            override fun invoke(value: Long): Long = value * other
        }
        data class Plus(val other: Int) : Operation() {
            override fun invoke(value: Long): Long = value + other
        }
    }

    companion object {
        private fun String.toOperation(): Operation = when {
            this == "new = old * old" -> Operation.Square
            startsWith("new = old * ") -> Operation.Times(substring(12).toInt())
            startsWith("new = old + ") -> Operation.Plus(substring(12).toInt())
            else -> throw IllegalArgumentException(this)
        }
    }
}
