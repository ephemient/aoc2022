import io.gitlab.arturbosch.detekt.Detekt
import org.gradle.api.plugins.ApplicationPlugin.APPLICATION_GROUP
import org.gradle.internal.component.external.model.ProjectDerivedCapability
import org.gradle.internal.component.external.model.TestFixturesSupport
import org.gradle.language.base.plugins.LifecycleBasePlugin.VERIFICATION_GROUP
import org.jetbrains.kotlin.gradle.plugin.mpp.KotlinNativeTargetWithHostTests
import org.jetbrains.kotlin.gradle.plugin.mpp.KotlinNativeTargetWithHostTestsPreset

plugins {
    kotlin("multiplatform")
    kotlin("plugin.allopen")
    alias(libs.plugins.dependency.updates)
    alias(libs.plugins.detekt)
    alias(libs.plugins.kotlinx.benchmark)
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
    jvm {
        compilations.create("bench")
        val jvmTestFixtures by configurations.creating {
            val parent = configurations.getByName("jvmTestRuntimeClasspath")
            extendsFrom(parent)
            isCanBeConsumed = true
            isCanBeResolved = false
            attributes {
                for (key in parent.attributes.keySet()) attribute(
                    key as Attribute<Any>,
                    parent.attributes.getAttribute(key)
                )
                attribute(Bundling.BUNDLING_ATTRIBUTE, objects.named(Bundling.EXTERNAL))
                attribute(Usage.USAGE_ATTRIBUTE, objects.named(Usage.JAVA_RUNTIME))
            }
            outgoing {
                capability(ProjectDerivedCapability(project, TestFixturesSupport.TEST_FIXTURES_FEATURE_NAME))
            }
        }
        dependencies {
            jvmTestFixtures(compilations.getByName("test").runtimeDependencyFiles)
        }
    }
    presets.withType<KotlinNativeTargetWithHostTestsPreset> {
        targetFromPreset(this) {
            binaries.executable {
                entryPoint("com.github.ephemient.aoc2022.main")
            }
            compilations.create("bench")
        }
    }

    sourceSets {
        val commonMain by getting
        val commonTest by getting {
            dependencies {
                implementation(kotlin("test"))
            }
        }
        val commonBench by creating {
            dependsOn(commonMain)
            dependencies {
                implementation(libs.kotlinx.benchmark)
            }
        }

        val jvmMain by getting {
            resources.srcDir(jvmResources)
        }
        getByName("jvmTest") {
            dependencies {
                implementation(kotlin("test-junit5"))
                implementation(libs.junit.jupiter.api)
                runtimeOnly(libs.junit.jupiter.engine)
            }
        }
        getByName("jvmBench") {
            dependsOn(commonBench)
            dependsOn(jvmMain)
        }

        val nativeMain by creating {
            dependsOn(commonMain)
        }
        val nativeTest by creating {
            dependsOn(commonTest)
        }
        val nativeBench by creating {
            dependsOn(commonBench)
        }
        targets.withType<KotlinNativeTargetWithHostTests> {
            val targetMain = getByName("${name}Main") {
                dependsOn(nativeMain)
            }
            getByName("${name}Test") {
                dependsOn(nativeTest)
            }
            getByName("${name}Bench") {
                dependsOn(nativeBench)
                dependsOn(targetMain)
            }
        }
    }
}

allOpen {
    annotation("org.openjdk.jmh.annotations.State")
}

benchmark {
    targets {
        register("jvmBench")
        kotlin.targets.withType<KotlinNativeTargetWithHostTests> {
            register("${name}Bench")
        }
    }

    configurations {
        named("main") {
            warmups = 1
            iterationTime = 1
            project.findProperty("benchmarkInclude")?.let { include(it.toString()) }
            project.findProperty("benchmarkExclude")?.let { exclude(it.toString()) }
        }
    }
}

val jvmJar by tasks.existing
val jvmRuntimeClasspath by configurations.existing

tasks.register<JavaExec>("runJvm") {
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
