plugins {
    application
    alias(libs.plugins.native.image)
}

application {
    mainClass.set("com.github.ephemient.aoc2022.MainKt")
}

graalvmNative {
    agent {
        enabled.set(true)
    }
    binaries {
        getByName("main") {
            imageName.set(rootProject.name)
            buildArgs.add("-H:IncludeResources=day.*\\.txt")
        }
        all {
            verbose.set(true)
        }
    }
}

dependencies {
    implementation(rootProject)
    testImplementation(testFixtures(projects.aoc2022))
    testImplementation(libs.graal.sdk)
    testImplementation(libs.junit.jupiter.api)
    testRuntimeOnly(libs.junit.jupiter.engine)
}

tasks.test {
    testClassesDirs = files(testClassesDirs, classpath) // Scan for test classes in classpath
    useJUnitPlatform()
}

