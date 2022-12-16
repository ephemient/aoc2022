package com.github.ephemient.aoc2022

expect class PriorityQueue<E : Any>(comparator: Comparator<in E>) {
    fun isEmpty(): Boolean

    fun add(element: E): Boolean

    @Throws(NoSuchElementException::class)
    fun remove(): E
}
