package com.github.ephemient.aoc2022

expect fun getTestInput(day: Int, extra: String = ""): List<String>

@OptIn(ExperimentalMultiplatform::class)
@OptionalExpectation
@Target(AnnotationTarget.CLASS, AnnotationTarget.FUNCTION)
expect annotation class SlowTest()
