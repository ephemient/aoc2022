// provided.js - generated by ephemient/aoc2022/Kotlin benchmarks, 2022-12-01 21:44:21.442354805

var providedBenchmarks = ["jvmTest"];

var providedBenchmarkStore = {
  "jvmTest": [
    {
      "jmhVersion": "1.21",
      "benchmark": "com.github.ephemient.aoc2022.Day1Bench.part1",
      "mode": "thrpt",
      "threads": 1,
      "forks": 1,
      "jvm": "/usr/lib/jvm/temurin-17-jdk-amd64/bin/java",
      "jvmArgs": [
        "-Dfile.encoding=UTF-8",
        "-Duser.country",
        "-Duser.language=en",
        "-Duser.variant"
      ],
      "jdkVersion": "17.0.5",
      "vmName": "OpenJDK 64-Bit Server VM",
      "vmVersion": "17.0.5+8",
      "warmupIterations": 1,
      "warmupTime": "1 s",
      "warmupBatchSize": 1,
      "measurementIterations": 5,
      "measurementTime": "1 s",
      "measurementBatchSize": 1,
      "primaryMetric": {
        "score": 29467.562602904705,
        "scoreError": 740.8316031529353,
        "scoreConfidence": [
          28726.73099975177,
          30208.39420605764
        ],
        "scorePercentiles": {
          "0.0": 29169.189983268592,
          "50.0": 29458.239954393335,
          "90.0": 29671.515055287524,
          "95.0": 29671.515055287524,
          "99.0": 29671.515055287524,
          "99.9": 29671.515055287524,
          "99.99": 29671.515055287524,
          "99.999": 29671.515055287524,
          "99.9999": 29671.515055287524,
          "100.0": 29671.515055287524
        },
        "scoreUnit": "ops/s",
        "rawData": [
          [
            29169.189983268592,
            29442.080559017657,
            29596.78746255645,
            29458.239954393335,
            29671.515055287524
          ]
        ]
      },
      "secondaryMetrics": {}
    },
    {
      "jmhVersion": "1.21",
      "benchmark": "com.github.ephemient.aoc2022.Day1Bench.part2",
      "mode": "thrpt",
      "threads": 1,
      "forks": 1,
      "jvm": "/usr/lib/jvm/temurin-17-jdk-amd64/bin/java",
      "jvmArgs": [
        "-Dfile.encoding=UTF-8",
        "-Duser.country",
        "-Duser.language=en",
        "-Duser.variant"
      ],
      "jdkVersion": "17.0.5",
      "vmName": "OpenJDK 64-Bit Server VM",
      "vmVersion": "17.0.5+8",
      "warmupIterations": 1,
      "warmupTime": "1 s",
      "warmupBatchSize": 1,
      "measurementIterations": 5,
      "measurementTime": "1 s",
      "measurementBatchSize": 1,
      "primaryMetric": {
        "score": 27654.621005743265,
        "scoreError": 295.5401886922542,
        "scoreConfidence": [
          27359.08081705101,
          27950.16119443552
        ],
        "scorePercentiles": {
          "0.0": 27523.839144075606,
          "50.0": 27675.962736533344,
          "90.0": 27718.5434106998,
          "95.0": 27718.5434106998,
          "99.0": 27718.5434106998,
          "99.9": 27718.5434106998,
          "99.99": 27718.5434106998,
          "99.999": 27718.5434106998,
          "99.9999": 27718.5434106998,
          "100.0": 27718.5434106998
        },
        "scoreUnit": "ops/s",
        "rawData": [
          [
            27656.38700550705,
            27523.839144075606,
            27718.5434106998,
            27698.372731900523,
            27675.962736533344
          ]
        ]
      },
      "secondaryMetrics": {}
    }
  ]
};
