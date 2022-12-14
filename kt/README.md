# [Advent of Code 2022](https://adventofcode.com/2022)
### my answers in [Kotlin](https://www.kotlinlang.org/) ![Kotlin CI](https://github.com/ephemient/aoc2022/workflows/Kotlin%20CI/badge.svg)

This project builds with [Gradle](https://gradle.org/).

Run the test suite:

```sh
./gradlew :allTests
```

Run [kotlinx.benchmark](https://github.com/Kotlin/kotlinx-benchmark) ([JMH](https://openjdk.java.net/projects/code-tools/jmh/)) benchmarks:

```sh
env aoc2022_data=.. ./gradlew :benchmark
```

Print solutions for the inputs provided in local data files:

```sh
env aoc2022_data=.. ./gradlew :runJvm :run{Debug,Release}Executable{Linux{X64,Arm64},MingwX86,Macos{X64,Arm64}}
```

Generate [Dokka](https://github.com/Kotlin/dokka) API documentation:

```sh
./gradlew :dokkaHtml
```

Run all checks, including [Detekt](https://detekt.github.io/) static code analysis and [ktlint](https://ktlint.github.io/) formatter:

```sh
./gradlew :check
```

Build/run/test/benchmark with [GraalVM native-image](https://www.graalvm.org/latest/reference-manual/native-image/):

```sh
$GRAALVM_HOME/bin/gu install native-image
./gradlew :graalvm:nativeCompile
./gradlew :graalvm:nativeRun
./gradlew :graalvm:nativeTest
./gradlew :graalvm:nativeJmhRun
```

Check for newer versions of dependencies:

```sh
./gradlew :dependencyUpdates
```
