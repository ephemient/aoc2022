@file:JvmName("JvmMainKt")

package com.github.ephemient.aoc2022

import java.io.File
import java.io.IOException

actual fun getInput(day: Int): List<String> {
    val fileName = "day$day.txt"
    val inputStream = try {
        System.getenv("aoc2022_datadir")?.let { File(File(it), fileName).inputStream() }
    } catch (_: IOException) {
        null
    } ?: checkNotNull(ClassLoader.getSystemClassLoader().getResourceAsStream(fileName)) { "No data for day $day" }
    return inputStream.bufferedReader().use { it.readLines() }
}
