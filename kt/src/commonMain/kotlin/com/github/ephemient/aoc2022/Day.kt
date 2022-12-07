package com.github.ephemient.aoc2022

@Target(AnnotationTarget.CLASS)
@Retention(AnnotationRetention.SOURCE)
annotation class Day {
    @Target(AnnotationTarget.FUNCTION)
    annotation class Part
}
