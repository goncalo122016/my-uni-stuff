import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm")
}

group = "com.li4"
version = "0.0.1-SNAPSHOT"

java { sourceCompatibility = JavaVersion.VERSION_17 }

repositories { mavenCentral() }

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
}

tasks.withType<KotlinCompile> {
    compilerOptions {
        freeCompilerArgs.add("-Xjsr305=strict")
        jvmTarget.set(org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_17)
    }
}
