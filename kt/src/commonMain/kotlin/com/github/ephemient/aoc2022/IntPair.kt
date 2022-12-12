package com.github.ephemient.aoc2022

data class IntPair(val first: Int, val second: Int)

infix fun Int.to(other: Int): IntPair = IntPair(this, other)
