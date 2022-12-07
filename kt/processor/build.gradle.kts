import io.gitlab.arturbosch.detekt.Detekt

plugins {
    kotlin("jvm")
    alias(libs.plugins.detekt)
}

dependencies {
    detektPlugins(libs.detekt.formatting)
    implementation(libs.kotlinpoet)
    implementation(libs.kotlinpoet.ksp)
    implementation(libs.ksp.api)
}

tasks.withType<Detekt>().configureEach {
    config.from(rootProject.file("detekt.yml"))
    buildUponDefaultConfig = true
    autoCorrect = System.getenv("CI").isNullOrEmpty()
    exclude { it.file.toPath().startsWith(buildDir.toPath()) }
}
tasks.register("detektAll") { dependsOn(tasks.withType<Detekt>()) }
tasks.check { dependsOn(tasks.withType<Detekt>()) }
