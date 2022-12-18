package com.github.ephemient.aoc2022.processor

import com.google.devtools.ksp.processing.CodeGenerator
import com.google.devtools.ksp.processing.KSPLogger
import com.google.devtools.ksp.processing.Resolver
import com.google.devtools.ksp.processing.SymbolProcessor
import com.google.devtools.ksp.processing.SymbolProcessorEnvironment
import com.google.devtools.ksp.processing.SymbolProcessorProvider
import com.google.devtools.ksp.symbol.KSAnnotated
import com.google.devtools.ksp.symbol.KSClassDeclaration
import com.google.devtools.ksp.symbol.KSFunctionDeclaration
import com.squareup.kotlinpoet.ARRAY
import com.squareup.kotlinpoet.FileSpec
import com.squareup.kotlinpoet.FunSpec
import com.squareup.kotlinpoet.MemberName
import com.squareup.kotlinpoet.ParameterizedTypeName.Companion.parameterizedBy
import com.squareup.kotlinpoet.STRING
import com.squareup.kotlinpoet.buildCodeBlock
import com.squareup.kotlinpoet.ksp.toClassName
import com.squareup.kotlinpoet.ksp.writeTo

class MainProcessor(private val codeGenerator: CodeGenerator, private val logger: KSPLogger) : SymbolProcessor {

    @Suppress("LongMethod")
    override fun process(resolver: Resolver): List<KSAnnotated> {
        val containers = resolver.getSymbolsWithAnnotation("com.github.ephemient.aoc2022.Day")
            .filterIsInstance<KSClassDeclaration>()
            .sortedWith(
                compareBy(
                    compareBy<String, Int?>(nullsLast(naturalOrder())) { name ->
                        name.removePrefix("Day").takeWhile { it.isDigit() }.toIntOrNull()
                    }.thenBy { it }
                ) { it.simpleName.asString() }
            )
            .toSet()
        val allParts = resolver.getSymbolsWithAnnotation("com.github.ephemient.aoc2022.Day.Part")
            .filterIsInstance<KSFunctionDeclaration>()
            .groupBy { it.parentDeclaration }
        for (parts in (allParts - containers).values) {
            for (part in parts) {
                logger.error("Containing class must be annotated with @com.github.ephemient.aoc2022.Day", part)
            }
        }

        val getInput = MemberName("com.github.ephemient.aoc2022", "getInput")
        if (containers.isNotEmpty()) {
            val mainCode = buildCodeBlock {
                for (container in containers) {
                    val id = container.simpleName.asString().removePrefix("Day")
                    val day = id.takeWhile { it.isDigit() }.toIntOrNull() ?: 0
                    val name = "day$day"
                    beginControlFlow("if (args.isEmpty() || %S in args)", id)
                    addStatement("val %N = %T(%M(%L))", name, container.toClassName(), getInput, day)
                    val parts = allParts[container].orEmpty()
                    addStatement("println(%S)", "Day $id")
                    if (parts.isEmpty()) {
                        logger.warn("No members annotated with @com.github.ephemient.aoc2022.Day.Part", container)
                    }
                    for (part in parts) addStatement("println(%N.%N())", name, part.simpleName.asString())
                    addStatement("println()")
                    endControlFlow()
                }
            }
            val mainSpec = FunSpec.builder("main")
                .addParameter("args", ARRAY.parameterizedBy(STRING))
                .addCode(mainCode)
                .build()
            FileSpec.builder("com.github.ephemient.aoc2022", "Main")
                .addFunction(mainSpec)
                .build()
                .writeTo(codeGenerator, true, containers.mapNotNull { it.containingFile })
        }

        return emptyList()
    }

    class Provider : SymbolProcessorProvider {
        override fun create(environment: SymbolProcessorEnvironment): SymbolProcessor =
            MainProcessor(environment.codeGenerator, environment.logger)
    }
}
