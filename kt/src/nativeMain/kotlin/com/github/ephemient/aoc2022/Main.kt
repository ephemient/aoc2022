package com.github.ephemient.aoc2022

import kotlinx.cinterop.*
import platform.posix.*

@OptIn(UnsafeNumber::class)
actual fun getInput(day: Int): List<String> {
    val dataDir = checkNotNull(getenv("aoc2022_datadir")?.toKString()?.ifEmpty { null }) { "missing \$aoc2022_datadir" }
    var offset = 0
    var buffer = ByteArray(2 * BUFSIZ)
    val fd = open("$dataDir/day$day.txt", O_RDONLY)
    check(fd != -1) {
        val errno = errno
        "open(\$aoc2022_data/day$day.txt): ${strerror(errno)?.toKString() ?: errno.toString()}"
    }
    try {
        return buildList {
            while (true) {
                val count = buffer.usePinned {
                    read(fd, it.addressOf(offset), (buffer.size - offset).coerceAtMost(BUFSIZ).convert())
                }
                when {
                    count < 0 -> {
                        val errno = errno
                        check(errno == EINTR) {
                            "read(\$aoc2022_data/day$day.txt): ${strerror(errno)?.toKString() ?: errno.toString()}"
                        }
                        continue
                    }
                    count > 0 -> {
                        var start = 0
                        val end = offset + count.toInt()
                        for (pos in offset until end) {
                            if (buffer[pos] == '\n'.code.toByte()) {
                                add(buffer.toKString(startIndex = start, endIndex = pos))
                                start = pos + 1
                            }
                        }
                        buffer.copyInto(buffer, startIndex = start, endIndex = end)
                        offset = end - start
                        if (buffer.size - offset < BUFSIZ) buffer = buffer.copyOf(buffer.size * 2)
                    }
                    else -> break
                }
            }
            if (offset != 0) add(buffer.toKString(endIndex = offset))
        }
    } finally {
        close(fd)
    }
}

actual fun trace(message: String) {
    if (getenv("TRACE")?.get(0) != '0'.code.toByte()) fputs("$message\n", stderr)
}
