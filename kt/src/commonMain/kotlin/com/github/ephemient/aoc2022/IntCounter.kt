package com.github.ephemient.aoc2022

class IntCounter<E> : AbstractMutableMap<E, Int>() {
    private val delegate = mutableMapOf<E, Entry<E>>()
    private var entrySet: EntrySet<E>? = null

    override val size: Int get() = delegate.size
    override fun isEmpty(): Boolean = delegate.isEmpty()
    override val keys: MutableSet<E> get() = delegate.keys
    override val entries: MutableSet<MutableMap.MutableEntry<E, Int>> =
        entrySet ?: EntrySet(delegate.entries).also { entrySet = it }
    override fun get(key: E): Int = delegate[key]?.value ?: 0
    override fun put(key: E, value: Int): Int = delegate.getOrPut(key) { Entry(key, 0) }.setValue(value)
    override fun remove(key: E): Int = delegate.remove(key)?.value ?: 0

    fun add(key: E, value: Int): Int {
        val entry = delegate.getOrPut(key) { Entry(key, 0) }
        return entry.setValue(entry.value + value)
    }

    private class Entry<E>(override val key: E, override var value: Int) : MutableMap.MutableEntry<E, Int> {
        override fun setValue(newValue: Int): Int = value.also { value = newValue }
    }

    private class EntrySet<E>(
        private val delegate: MutableSet<MutableMap.MutableEntry<E, Entry<E>>>,
    ) : AbstractMutableSet<MutableMap.MutableEntry<E, Int>>() {
        override val size: Int get() = delegate.size
        override fun isEmpty(): Boolean = delegate.isEmpty()
        override fun iterator() = object : MutableIterator<MutableMap.MutableEntry<E, Int>> {
            private val delegate = this@EntrySet.delegate.iterator()
            override fun hasNext(): Boolean = delegate.hasNext()
            override fun next(): MutableMap.MutableEntry<E, Int> = delegate.next().value
            override fun remove() {
                delegate.remove()
            }
        }
        override fun add(element: MutableMap.MutableEntry<E, Int>): Boolean {
            throw UnsupportedOperationException()
        }
        override fun remove(element: MutableMap.MutableEntry<E, Int>): Boolean {
            throw UnsupportedOperationException()
        }
    }
}
