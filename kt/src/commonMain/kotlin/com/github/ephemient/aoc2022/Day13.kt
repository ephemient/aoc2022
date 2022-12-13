package com.github.ephemient.aoc2022

@Day
class Day13(lines: List<String>) {
    private val packets = lines.mapNotNull { if (it.isNotEmpty()) it.toPacket() else null }

    @Day.Part
    fun part1(): Int = packets.chunked(2) { (a, b) -> a <= b }.withIndex().filter { it.value }.sumOf { it.index + 1 }

    @Day.Part
    fun part2(): Int {
        var x = 1
        var y = 1
        for (packet in packets) {
            when {
                packet < a -> x++
                packet < b -> y++
            }
        }
        return x * (x + y)
    }

    private sealed class Packet : Comparable<Packet> {
        data class Literal(val value: Int) : Packet() {
            override fun compareTo(other: Packet): Int = when (other) {
                is Literal -> this.value compareTo other.value
                is List -> List(listOf(this)) compareTo other
            }
            override fun hashCode(): Int = value
        }

        data class List(val list: kotlin.collections.List<Packet>) : Packet() {
            override fun compareTo(other: Packet): Int {
                return when (other) {
                    is Literal -> this compareTo List(listOf(other))
                    is List -> {
                        val i = this.list.iterator()
                        val j = other.list.iterator()
                        while (i.hasNext() && j.hasNext()) {
                            val comparison = i.next() compareTo j.next()
                            if (comparison != 0) return comparison
                        }
                        i.hasNext() compareTo j.hasNext()
                    }
                }
            }
            override fun hashCode(): Int = list.fold(0) { acc, value -> 31 * acc + value.hashCode() }
        }

        override fun equals(other: Any?): Boolean = other is Packet && compareTo(other) == 0
        abstract override fun hashCode(): Int
    }

    companion object {
        private val a: Packet = Packet.List(listOf(Packet.List(listOf(Packet.Literal(2)))))
        private val b: Packet = Packet.List(listOf(Packet.List(listOf(Packet.Literal(6)))))

        private val parser = DeepRecursiveFunction<IndexedValue<String>, IndexedValue<Packet>> { (startIndex, string) ->
            if (string[startIndex] == '[') {
                var index = startIndex + 1
                val list = buildList {
                    while (string[index] != ']') {
                        val (endIndex, value) = callRecursive(IndexedValue(index, string))
                        add(value)
                        index = if (string[endIndex] == ',') endIndex + 1 else endIndex
                    }
                }
                IndexedValue(index + 1, Packet.List(list))
            } else {
                var index = startIndex + 1
                while (index < string.length && string[index] in '0'..'9') index++
                IndexedValue(index, Packet.Literal(string.substring(startIndex, index).toInt()))
            }
        }

        private fun String.toPacket(): Packet {
            val (index, packet) = parser.invoke(IndexedValue(0, this))
            require(index == length)
            return packet
        }
    }
}
