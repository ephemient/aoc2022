import org.graalvm.buildtools.gradle.internal.agent.AgentConfigurationFactory
import org.graalvm.buildtools.gradle.tasks.NativeRunTask
import org.gradle.internal.component.external.model.ProjectDerivedCapability

plugins {
    application
    alias(libs.plugins.native.image)
}

application {
    mainClass.set("com.github.ephemient.aoc2022.MainKt")
}

val jmhJar by configurations.creating {
    isCanBeConsumed = false
    isCanBeResolved = true
}

val jmhRun by tasks.registering(JavaExec::class) {
    classpath(jmhJar)
    mainClass.set("org.openjdk.jmh.Main")
    args("-f", "0", "-r", "1", "-w", "1", "-rf", "json", "-rff", "/dev/null", "-wi", "0", "-i", "1")
}

val copyJmhRunMetadata by tasks.registering(Sync::class) {
    destinationDir = File(buildDir, "generated/resources/jmhRunMetadata")
    into("META-INF/native-image") {
        from(files(AgentConfigurationFactory.getAgentOutputDirectoryForTask(layout, "jmhRun")).builtBy(jmhRun))
    }
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
        create("jmh") {
            imageName.set("${rootProject.name}-jmh")
            buildArgs.add("--initialize-at-build-time=org.openjdk.jmh.infra,org.openjdk.jmh.util.Utils,org.openjdk.jmh.runner.InfraControl,org.openjdk.jmh.runner.InfraControlL0,org.openjdk.jmh.runner.InfraControlL1,org.openjdk.jmh.runner.InfraControlL2,org.openjdk.jmh.runner.InfraControlL3,org.openjdk.jmh.runner.InfraControlL4")
            mainClass.set("org.openjdk.jmh.Main")
            classpath.from(jmhJar, copyJmhRunMetadata)
        }
        all {
            verbose.set(true)
        }
    }
}

tasks.named<NativeRunTask>("nativeJmhRun") {
    val reportFile = layout.buildDirectory.file("reports/benchmarks/jmhBench.json")
    outputs.file(reportFile).withPropertyName("reportFile")
    runtimeArgs.addAll("-f", "0", "-r", "1", "-w", "1", "-wi", "0")
    runtimeArgs.addAll(provider { listOf("-rf", "json", "-rff", reportFile.get().asFile.absolutePath) })
}

dependencies {
    implementation(rootProject)
    jmhJar(
        projects.aoc2022.apply {
            capabilities {
                requireCapabilities(ProjectDerivedCapability(dependencyProject, "jmh"))
            }
        }
    )
    testImplementation(testFixtures(projects.aoc2022))
    testImplementation(libs.graal.sdk)
    testImplementation(libs.junit.jupiter.api)
    testRuntimeOnly(libs.junit.jupiter.engine)
}

tasks.test {
    testClassesDirs = files(testClassesDirs, classpath) // Scan for test classes in classpath
    useJUnitPlatform()
}
