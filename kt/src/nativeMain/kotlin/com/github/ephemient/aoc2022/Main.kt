package com.github.ephemient.aoc2022

import kotlinx.cinterop.*
import platform.posix.*

@OptIn(UnsafeNumber::class)
actual fun getInput(day: Int): List<String> {
    val dataDir = checkNotNull(getenv("aoc2022_data")) { "missing \$aoc2022_data" }.toKString()
    val fd = open("$dataDir/day$day.txt", O_RDONLY)
    check(fd != -1) {
        val errno = errno
        "open(\$aoc2022_data/day$day.txt): ${strerror(errno)?.toKString() ?: errno.toString()}"
    }
    var length = 0
    var buffer: ByteArray
    try {
        buffer = ByteArray(cValue<stat> { fstat(fd, ptr) }.size.coerceAtLeast(0) + BUFSIZ)
        do {
            val readSize = (buffer.size - length).coerceAtLeast(BUFSIZ)
            val count = buffer.usePinned { read(fd, it.addressOf(length), readSize.convert()) }
            check(count >= 0) {
                val errno = errno
                "read(\$aoc2022_data/day$day.txt): ${strerror(errno)?.toKString() ?: errno.toString()}"
            }
            length += count.toInt()
            if (length + BUFSIZ > buffer.size) {
                val newSize = maxOf(length + BUFSIZ, Int.MIN_VALUE ushr buffer.size.countLeadingZeroBits() - 1)
                buffer = buffer.copyOf(newSize)
            }
        } while (count >= readSize)
    } finally {
        close(fd)
    }
    return buildList {
        var i = 0
        while (i < length) {
            var j = i
            while (buffer[j] != '\n'.code.toByte()) {
                if (++j >= length) break
            }
            add(buffer.toKString(i, j))
            i = j + 1
        }
    }
}
