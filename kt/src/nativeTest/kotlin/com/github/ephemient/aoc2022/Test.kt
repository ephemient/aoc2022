package com.github.ephemient.aoc2022

import kotlinx.cinterop.*
import platform.posix.*
import kotlin.test.Ignore

actual fun getTestInput(day: Int, extra: String): List<String> {
    val dataDir = checkNotNull(
        getenv("aoc2022_test_datadir")?.toKString()?.ifEmpty { null }
    ) { "missing \$aoc2022_test_datadir" }
    val filename = "day$day$extra.txt"
    val file = checkNotNull(fopen("$dataDir/$filename", "r")) {
        val errno = errno
        "fopen(\$aoc2022_test_datadir/$filename): ${strerror(errno)?.toKString() ?: errno.toString()}"
    }
    try {
        return buildList {
            var previousNewline = true
            val buffer = ByteArray(BUFSIZ)
            while (buffer.usePinned { fgets(it.addressOf(0), buffer.size, file) } != null) {
                val eos = buffer.indexOf(0)
                val nextNewline = eos > 0 && buffer[eos - 1] == '\n'.code.toByte()
                val eol = if (nextNewline) eos - 1 else eos
                if (previousNewline) {
                    add(buffer.toKString(endIndex = eol))
                } else {
                    this[lastIndex] = last() + buffer.toKString(endIndex = eol)
                }
                previousNewline = nextNewline
            }
            if (ferror(file) != 0) {
                val errno = errno
                fprintf(
                    stderr,
                    "fgets(\$aoc2022_test_datadir/$filename) = ${strerror(errno)?.toKString() ?: errno.toString()}"
                )
            }
        }
    } finally {
        fclose(file)
    }
}

actual typealias SlowTest = Ignore
