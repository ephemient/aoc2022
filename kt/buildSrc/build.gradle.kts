plugins {
    `embedded-kotlin`
}

repositories {
    mavenCentral()
}

dependencies {
    implementation(gradleApi())
    implementation(kotlin("gradle-plugin", libs.versions.kotlin.get()))
}
