import java.util.*
import java.io.*
import java.math.*

fun main(args : Array<String>) {
    val input = Scanner(System.`in`)
    val regs: Array<Int> = Array(4, { i -> input.nextInt() })
    val n = input.nextInt()
    if (input.hasNextLine()) {
        input.nextLine()
    }
    val instructions = IntRange(1, n).map { input.nextLine() }

    var sp = 0;
    while (sp < instructions.size) {
        val instr = instructions[sp++]
        val split = instr.split(" ")
        when (split[0]) {
            "MOV" -> {
                val reg = split[1].first() - 'a'
                regs[reg] = opValue(split[2], regs)
            }
            "ADD" -> {
                val reg = split[1].first() - 'a'
                regs[reg] = opValue(split[2], regs) + opValue(split[3], regs)
            }
            "SUB" -> {
                val reg = split[1].first() - 'a'
                regs[reg] = opValue(split[2], regs) - opValue(split[3], regs)
            }
            "JNE" -> {
                sp = if (opValue(split[2], regs) == opValue(split[3], regs)) sp else split[1].toInt()
            }
        }
    }

    println(regs.joinToString(" "))
}

fun opValue(op: String, regs: Array<Int>): Int {
    return if (op.all { c -> c.isDigit() || c == '-' }) op.toInt() else regs[op.first() - 'a']
}