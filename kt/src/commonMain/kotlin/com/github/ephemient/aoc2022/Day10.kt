package com.github.ephemient.aoc2022

@Day
class Day10(lines: List<String>) {
    private val instructions = lines.map { it.toInstruction() }

    @Day.Part
    fun part1(): Int {
        val iterator = instructions.iterator()
        var cycle = 0
        var x = 1
        return taps.sumOf { i ->
            var lastX = x
            while (cycle < i) {
                lastX = x
                when (val instruction = iterator.next()) {
                    Instruction.Noop -> cycle++
                    is Instruction.AddX -> {
                        cycle += 2
                        x += instruction.dx
                    }
                }
            }
            i * lastX
        }
    }

    @Day.Part
    fun part2(): String = buildString(245) {
        val iterator = instructions.iterator()
        var cycle = 0
        var x = 1
        for (row in 0 until 240 step 40) {
            if (row != 0) append('\n')
            repeat(40) { col ->
                append(if (x - col in -1..1) '\u2593' else '\u2591')
                while (cycle < row + col) {
                    when (val instruction = iterator.next()) {
                        Instruction.Noop -> cycle++
                        is Instruction.AddX -> {
                            cycle += 2
                            x += instruction.dx
                        }
                    }
                }
            }
        }
    }

    private sealed class Instruction {
        object Noop : Instruction()
        data class AddX(val dx: Int) : Instruction()
    }

    companion object {
        private val taps get() = intArrayOf(20, 60, 100, 140, 180, 220)

        private fun String.toInstruction(): Instruction = when {
            this == "noop" -> Instruction.Noop
            else -> Instruction.AddX(removePrefix("addx ").toInt())
        }
    }
}
