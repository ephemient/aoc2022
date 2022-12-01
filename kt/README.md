# [Advent of Code 2022](https://adventofcode.com/2022)
### my answers in [Kotlin](https://www.kotlinlang.org/) ![Kotlin CI](https://github.com/ephemient/aoc2022/workflows/Kotlin%20CI/badge.svg)

This project builds with [Gradle](https://gradle.org/).

Run the test suite:

```sh
./gradlew allTests
```

Run [kotlinx.benchmark](https://github.com/Kotlin/kotlinx-benchmark) ([JMH](https://openjdk.java.net/projects/code-tools/jmh/)) benchmarks:

```sh
./gradlew benchmark
```

Print solutions for the inputs provided in local data files:

```sh
./gradlew jvmRun
```

Run all checks, including [Detekt](https://detekt.github.io/) static code analysis and [ktlint](https://ktlint.github.io/) formatter:

```sh
./gradlew check
```

Check for newer versions of dependencies:

```sh
./gradlew dependencyUpdates
```
