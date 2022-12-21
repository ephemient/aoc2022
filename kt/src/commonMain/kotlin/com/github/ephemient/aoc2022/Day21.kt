package com.github.ephemient.aoc2022

import kotlin.math.abs

@Day
class Day21(lines: List<String>) {
    private val monkeys = buildMap {
        for (line in lines) {
            val (name, definition) = line.split(": ", limit = 2)
            this[name] = definition.toIntOrNull()?.let { Expr.Literal(it) }
                ?: definition.split(' ', limit = 3).let { (lhs, symbol, rhs) ->
                    val op = when (symbol) {
                        "+" -> Operation.Add
                        "-" -> Operation.Subtract
                        "*" -> Operation.Multiply
                        "/" -> Operation.Divide
                        else -> throw IllegalArgumentException(symbol)
                    }
                    Expr.Binary(op, lhs, rhs)
                }
        }
    }

    @Day.Part
    fun part1(): Long {
        val cache = mutableMapOf<String, Long>()
        return DeepRecursiveFunction<String, Long> { name ->
            cache.getOrPut(name) {
                when (val expr = monkeys.getValue(name)) {
                    is Expr.Literal -> expr.value.toLong()
                    is Expr.Binary -> when (expr.op) {
                        Operation.Add -> callRecursive(expr.lhs) + callRecursive(expr.rhs)
                        Operation.Subtract -> callRecursive(expr.lhs) - callRecursive(expr.rhs)
                        Operation.Multiply -> callRecursive(expr.lhs) * callRecursive(expr.rhs)
                        Operation.Divide -> callRecursive(expr.lhs) / callRecursive(expr.rhs)
                    }
                }
            }
        }("root")
    }

    @Day.Part
    fun part2(): Long {
        val (_, lhs, rhs) = monkeys.getValue("root") as Expr.Binary
        val cache = mutableMapOf("humn" to (Rational(1) to Rational(0)))
        val eval = DeepRecursiveFunction<String, Pair<Rational, Rational>> { name ->
            cache.getOrPut(name) {
                when (val expr = monkeys.getValue(name)) {
                    is Expr.Literal -> Rational(0) to Rational(expr.value.toLong())
                    is Expr.Binary -> {
                        val (a, b) = callRecursive(expr.lhs)
                        val (c, d) = callRecursive(expr.rhs)
                        when (expr.op) {
                            Operation.Add -> a + c to b + d
                            Operation.Subtract -> a - c to b - d
                            Operation.Multiply -> when {
                                a.numerator == 0L -> b * c to b * d
                                c.numerator == 0L -> a * d to b * d
                                else -> TODO()
                            }
                            Operation.Divide -> if (c.numerator == 0L) a / d to b / d else TODO()
                        }
                    }
                }
            }
        }
        val (m, b) = eval(lhs)
        val (n, c) = eval(rhs)
        val x = (c - b) / (m - n)
        check(x.denominator == 1L)
        return x.numerator
    }

    private enum class Operation {
        Add, Subtract, Multiply, Divide,
    }

    private sealed class Expr {
        data class Literal(val value: Int) : Expr()
        data class Binary(val op: Operation, val lhs: String, val rhs: String) : Expr()
    }

    private data class Rational(val numerator: Long, val denominator: Long = 1) {
        init {
            require(denominator > 0)
        }

        operator fun plus(other: Rational): Rational {
            val denominator = this.denominator * other.denominator / gcd(this.denominator, other.denominator)
            val numerator = this.numerator * (denominator / this.denominator) +
                other.numerator * (denominator / other.denominator)
            val gcd = abs(gcd(numerator, denominator))
            return Rational(numerator / gcd, denominator / gcd)
        }

        operator fun minus(other: Rational): Rational {
            val denominator = this.denominator * other.denominator / gcd(this.denominator, other.denominator)
            val numerator = this.numerator * (denominator / this.denominator) -
                other.numerator * (denominator / other.denominator)
            val gcd = abs(gcd(numerator, denominator))
            return Rational(numerator / gcd, denominator / gcd)
        }

        operator fun times(other: Rational): Rational {
            val numerator = this.numerator * other.numerator
            val denominator = this.denominator * other.denominator
            val gcd = abs(gcd(numerator, denominator))
            return Rational(numerator / gcd, denominator / gcd)
        }

        operator fun div(other: Rational): Rational {
            require(other.numerator != 0L)
            val numerator = this.numerator * other.denominator
            val denominator = this.denominator * other.numerator
            val gcd = gcd(numerator, denominator).let { if (it < 0 != denominator < 0) -it else it }
            return Rational(numerator / gcd, denominator / gcd)
        }
    }

    companion object {
        private fun gcd(a: Long, b: Long): Long {
            var x = a
            var y = b
            while (y != 0L) {
                x = y.also { y = x % y }
            }
            return x
        }
    }
}
