package com.github.ephemient.aoc2022

import kotlin.jvm.JvmName

@Day
class Day8(private val lines: List<String>) {
    private val width = lines.maxOf { it.length }

    @Day.Part
    fun part1(): Int {
        val visibilities = lines.map { line ->
            MutableList(line.length) { false }.apply {
                scanVisibility(line, this)
                scanVisibility(line.asReversed(), this.asReversed())
            }
        }
        repeat(width) {
            scanVisibility(lines.column(it), visibilities.column(it))
            scanVisibility(lines.column(it).asReversed(), visibilities.column(it).asReversed())
        }
        return visibilities.sumOf { it.count { it } }
    }

    @Day.Part
    fun part2(): Int {
        val scores = lines.map { line ->
            MutableList(line.length) { 1 }.apply {
                scanHorizon(line, this)
                scanHorizon(line.asReversed(), this.asReversed())
            }
        }
        repeat(width) {
            scanHorizon(lines.column(it), scores.column(it))
            scanHorizon(lines.column(it).asReversed(), scores.column(it).asReversed())
        }
        return scores.maxOf { it.max() }
    }
}

private fun scanVisibility(from: CharSequence, into: MutableList<Boolean>) {
    var max = '\u0000'
    from.forEachIndexed { index, char ->
        if (index == 0 || char > max) {
            into[index] = true
            max = char
        }
    }
}

private fun scanHorizon(from: CharSequence, into: MutableList<Int>) {
    from.forEachIndexed { index, char ->
        var horizon = 0
        for (i in index - 1 downTo 0) {
            if (from[i] >= char) {
                horizon = i
                break
            }
        }
        into[index] *= index - horizon
    }
}

private fun CharSequence.asReversed(): CharSequence = object : CharSequence {
    override val length: Int get() = this@asReversed.length
    override fun get(index: Int): Char = this@asReversed[length - 1 - index]
    override fun subSequence(startIndex: Int, endIndex: Int): CharSequence =
        this@asReversed.subSequence(length - endIndex, length - startIndex).asReversed()
    override fun toString(): String = this@asReversed.toString().reversed()
}

@JvmName("charSequenceColumn")
private fun List<CharSequence>.column(column: Int): CharSequence = object : CharSequence {
    override val length: Int get() = this@column.size
    override fun get(index: Int): Char = this@column[index][column]
    override fun subSequence(startIndex: Int, endIndex: Int): CharSequence = buildString(endIndex - startIndex) {
        for (i in startIndex until endIndex) append(get(i))
    }
    override fun toString(): String = buildString(length) {
        for (i in this@column.indices) append(get(i))
    }
}

@JvmName("mutableListColumn")
private fun <E> List<MutableList<E>>.column(column: Int): MutableList<E> = object : AbstractMutableList<E>() {
    override val size: Int get() = this@column.size
    override fun get(index: Int): E = this@column[index][column]
    override fun set(index: Int, element: E): E = this@column[index].set(column, element)
    override fun add(index: Int, element: E) = throw UnsupportedOperationException()
    override fun removeAt(index: Int): E = throw UnsupportedOperationException()
}
