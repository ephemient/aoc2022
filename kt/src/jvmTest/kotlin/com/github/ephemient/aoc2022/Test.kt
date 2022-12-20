package com.github.ephemient.aoc2022

import java.io.File
import java.io.IOException

actual fun getTestInput(day: Int, extra: String): List<String> {
    val fileName = "day$day$extra.txt"
    val inputStream = try {
        System.getenv("aoc2022_test_datadir")?.ifEmpty { null }?.let { File(File(it), fileName).inputStream() }
    } catch (_: IOException) {
        null
    } ?: checkNotNull(ClassLoader.getSystemClassLoader().getResourceAsStream(fileName)) { "No data for day $day$extra" }
    return inputStream.bufferedReader().use { it.readLines() }
}
