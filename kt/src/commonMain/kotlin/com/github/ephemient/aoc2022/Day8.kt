package com.github.ephemient.aoc2022

@Day
class Day8(private val lines: List<String>) {
    private val width = lines.maxOf { it.length }

    @Day.Part
    fun part1(): Int {
        val visibilities = lines.map { line ->
            BooleanArray(line.length).apply {
                var index = 0
                for (value in line.iterator().scanVisibility()) {
                    if (value) set(index, true)
                    index++
                }
                for (value in line.reverseIterator().scanVisibility()) {
                    --index
                    if (value) set(index, true)
                }
            }
        }
        repeat(width) {
            var index = 0
            for (value in lines.columnIterator(it).scanVisibility()) {
                if (value) visibilities[index][it] = true
                index++
            }
            for (value in lines.asReversed().columnIterator(it).scanVisibility()) {
                --index
                if (value) visibilities[index][it] = true
            }
        }
        return visibilities.sumOf { it.count { it } }
    }

    @Day.Part
    fun part2(): Int {
        val scores = lines.map { line ->
            IntArray(line.length).apply {
                var index = 0
                for (value in line.iterator().scanScore()) this[index++] = value
                for (value in line.reverseIterator().scanScore()) this[--index] *= value
            }
        }
        repeat(width) {
            var index = 0
            for (value in lines.columnIterator(it).scanScore()) scores[index++][it] *= value
            for (value in lines.asReversed().columnIterator(it).scanScore()) scores[--index][it] *= value
        }
        return scores.maxOf { it.max() }
    }
}

private fun CharIterator.scanVisibility(): BooleanIterator = object : BooleanIterator() {
    private var isFirst = true
    private var max = Char.MIN_VALUE
    override fun hasNext(): Boolean = this@scanVisibility.hasNext()
    override fun nextBoolean(): Boolean {
        val char = nextChar()
        val boolean = if (isFirst) true.also { isFirst = false } else char > max
        if (boolean) max = char
        return boolean
    }
}

private fun CharIterator.scanScore(): IntIterator = object : IntIterator() {
    private val seen = StringBuilder()
    private var index = 0
    override fun hasNext(): Boolean = this@scanScore.hasNext()
    override fun nextInt(): Int {
        val char = nextChar()
        val int = index++ - seen.indexOfLast { it >= char }.coerceAtLeast(0)
        seen.append(char)
        return int
    }
}

private fun CharSequence.reverseIterator(): CharIterator = object : CharIterator() {
    private var index = length
    override fun hasNext(): Boolean = index > 0
    override fun nextChar(): Char = get(--index)
}

private fun List<CharSequence>.columnIterator(column: Int): CharIterator = object : CharIterator() {
    private var index = 0
    override fun hasNext(): Boolean = index < size
    override fun nextChar(): Char = get(index++)[column]
}
