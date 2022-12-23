package com.github.ephemient.aoc2022

expect fun getInput(day: Int): List<String>

expect fun assert(condition: () -> Boolean)

expect fun assert(condition: () -> Boolean, lazyMessage: () -> Any)

expect fun trace(message: String)
