package com.github.ephemient.aoc2022

data class IntPair(val first: Int, val second: Int) {
    override fun toString(): String = "($first,$second)"
}

infix fun Int.to(other: Int): IntPair = IntPair(this, other)

data class IntTriple(val first: Int, val second: Int, val third: Int) {
    override fun toString(): String = "($first,$second,$third)"
}
