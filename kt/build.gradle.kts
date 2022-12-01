import io.gitlab.arturbosch.detekt.Detekt
import org.gradle.api.plugins.ApplicationPlugin.APPLICATION_GROUP
import org.gradle.language.base.plugins.LifecycleBasePlugin.VERIFICATION_GROUP

plugins {
    kotlin("multiplatform")
    alias(libs.plugins.dependency.updates)
    alias(libs.plugins.detekt)
}

dependencies {
    detektPlugins(libs.detekt.formatting)
}

val jvmResources by tasks.registering(Sync::class) {
    from(rootDir.parentFile)
    into(layout.buildDirectory.dir("generated/source/$name"))
    include("day*.txt")
}

kotlin {
    jvm()

    sourceSets {
        getByName("commonTest") {
            dependencies {
                implementation(kotlin("test"))
            }
        }

        getByName("jvmMain") {
            resources.srcDir(jvmResources)
        }
        getByName("jvmTest") {
            dependencies {
                implementation(kotlin("test-junit5"))
                implementation(libs.junit.jupiter.api)
                runtimeOnly(libs.junit.jupiter.engine)
            }
        }
    }
}

val jvmJar by tasks.existing
val jvmRuntimeClasspath by configurations.existing

tasks.register<JavaExec>("jvmRun") {
    description = "Runs this project as a JVM application"
    group = APPLICATION_GROUP
    classpath(jvmJar, jvmRuntimeClasspath)
    mainClass.set("com.github.ephemient.aoc2022.MainKt")
}

tasks.named<Test>("jvmTest") {
    useJUnitPlatform()
}

val detektKotlinScripts by tasks.registering(Detekt::class) {
    group = VERIFICATION_GROUP
    description = "Run detekt analysis for Kotlin scripts"
    source(files().apply { from(layout.projectDirectory.asFileTree.matching { include("*.kts") }) })
}

tasks.withType<Detekt>().configureEach {
    config.from("detekt.yml")
    buildUponDefaultConfig = true
    autoCorrect = System.getenv("CI").isNullOrEmpty()
    exclude { it.file.toPath().startsWith(buildDir.toPath()) }
}
tasks.register("detektAll") { dependsOn(tasks.withType<Detekt>()) }
tasks.check { dependsOn(tasks.withType<Detekt>()) }

tasks.dependencyUpdates {
    revision = "release"
    gradleReleaseChannel = "current"
}
