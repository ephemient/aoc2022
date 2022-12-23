package com.github.ephemient.aoc2022

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
    fun part1(): Long = DeepRecursiveFunction<String, Long> { name ->
        when (val expr = monkeys.getValue(name)) {
            is Expr.Literal -> expr.value.toLong()
            is Expr.Binary -> when (expr.op) {
                Operation.Add -> callRecursive(expr.lhs) + callRecursive(expr.rhs)
                Operation.Subtract -> callRecursive(expr.lhs) - callRecursive(expr.rhs)
                Operation.Multiply -> callRecursive(expr.lhs) * callRecursive(expr.rhs)
                Operation.Divide -> callRecursive(expr.lhs) / callRecursive(expr.rhs)
            }
        }
    }("root")

    @Day.Part
    fun part2(): Long {
        val (_, lhs, rhs) = monkeys.getValue("root") as Expr.Binary
        val eval = DeepRecursiveFunction<String, Pair<Rational, Rational>> { name ->
            if (name == "humn") return@DeepRecursiveFunction Rational(1) to Rational(0)
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
            assert { gcd(numerator, denominator) == 1L }
        }

        operator fun plus(other: Rational): Rational {
            val gcd1 = gcd(this.denominator, other.denominator)
            val multiplier = this.denominator / gcd1
            val denominator = multiplier * other.denominator
            val numerator = this.numerator * (other.denominator / gcd1) + other.numerator * multiplier
            val gcd2 = gcd(numerator, denominator)
            return Rational(numerator / gcd2, denominator / gcd2)
        }

        operator fun minus(other: Rational): Rational {
            val gcd1 = gcd(this.denominator, other.denominator)
            val multiplier = this.denominator / gcd1
            val denominator = multiplier * other.denominator
            val numerator = this.numerator * (other.denominator / gcd1) - other.numerator * multiplier
            val gcd2 = gcd(numerator, denominator)
            return Rational(numerator / gcd2, denominator / gcd2)
        }

        operator fun times(other: Rational): Rational {
            val gcd1 = gcd(this.numerator, other.denominator)
            val gcd2 = gcd(other.numerator, this.denominator)
            val numerator = (this.numerator / gcd1) * (other.numerator / gcd2)
            val denominator = (this.denominator / gcd2) * (other.denominator / gcd1)
            return Rational(numerator, denominator)
        }

        operator fun div(other: Rational): Rational {
            require(other.numerator != 0L)
            val gcd1 = gcd(this.numerator, other.numerator)
            val gcd2 = gcd(other.denominator, this.denominator)
            val numerator = (this.numerator / gcd1) * (other.denominator / gcd2)
            val denominator = (this.denominator / gcd2) * (other.numerator / gcd1)
            return Rational(numerator, denominator)
        }
    }

    companion object {
        private fun gcd(a: Long, b: Long): Long {
            var x = a
            var y = b
            while (y != 0L) x = y.also { y = x % y }
            return if (x < 0 != b < 0) -x else x
        }
    }
}
