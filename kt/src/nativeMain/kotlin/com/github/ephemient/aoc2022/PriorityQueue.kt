package com.github.ephemient.aoc2022

actual class PriorityQueue<E : Any> actual constructor(private val comparator: Comparator<in E>) {
    private var storage: MutableList<E> = mutableListOf()

    actual fun isEmpty(): Boolean = storage.isEmpty()

    actual fun add(element: E): Boolean {
        storage.add(element)
        var i = storage.lastIndex
        while (i > 0) {
            val j = (i - 1) / 2
            val a = storage[j]
            val b = storage[i]
            if (comparator.compare(a, b) <= 0) break
            storage[i] = a
            storage[j] = b
            i = j
        }
        return true
    }

    @Throws(NoSuchElementException::class)
    actual fun remove(): E {
        val first = storage.first()
        val last = storage.removeLast()
        if (storage.isNotEmpty()) {
            storage[0] = last
            var i = 0
            while (2 * i + 1 < storage.lastIndex) {
                val j = if (2 * i + 2 < storage.lastIndex) {
                    if (comparator.compare(storage[2 * i + 1], storage[2 * i + 2]) < 0) 2 * i + 1 else 2 * i + 2
                } else 2 * i + 1
                if (comparator.compare(storage[i], storage[j]) <= 0) break
                storage[i] = storage[j].also { storage[j] = storage[i] }
                i = j
            }
        }
        return first
    }
}
