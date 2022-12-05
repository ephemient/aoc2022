enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")

pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
    }

    val parseToml = Gradle::class.java.classLoader.loadClass("org.tomlj.Toml")
        .getMethod("parse", java.io.InputStream::class.java)
    val toml = file("gradle/libs.versions.toml").inputStream().use { parseToml.invoke(null, it) }
    val versions = toml.withGroovyBuilder {
        "getTable"("versions")?.withGroovyBuilder {
            ("keySet"() as Set<*>).associateWith { "get"(listOf(it)).toString() }
        }
    }.orEmpty()
    val kotlinVersion = versions["kotlin"]

    plugins {
        resolutionStrategy {
            eachPlugin {
                if (requested.id.id.startsWith("org.jetbrains.kotlin.")) useVersion(kotlinVersion)
            }
        }
    }
}

gradle.afterProject {
    repositories {
        mavenCentral()
    }
}

rootProject.name = "aoc2022"
include("graalvm")
