import java.lang.Math.max
import java.util.*

fun main(args : Array<String>) {
//    val input = Scanner(System.`in`)
//    val N1 = input.nextInt()
//    val N2 = input.nextInt()
//    val S1 = input.next().reversed()
//    val S2 = input.next()
//    val T = input.nextInt()

    val S1 = "A".reversed()
    val S2 = "DEF"
    val T = 4

    println(ants(S1, S2, T))
}

private fun ants(S1: String, S2: String, T: Int):String {
    val N1 = S1.length
    val N2 = S2.length

    if (T >= N1 + N2) {
        return (S2 + spaces(T - N1 - N2 + 1) + S1)
    }

    val n2Spaces = N1 - T
    val resLen = S1.length + S2.length + Math.abs(n2Spaces)
    val wrappedS2 = spaces(n2Spaces) + S2 + spaces(resLen - N2)

    val wrappedS1 = spaces(- n2Spaces) + S1 + spaces(resLen - N1)
    System.err.println("'$wrappedS1'")
    System.err.println("'$wrappedS2'")
    val result:List<String> = wrappedS1.zip(wrappedS2, { a, b ->
        if (a == ' ')
            if (b == ' ') " " else b.toString()
        else if (b == ' ') a.toString() else b.toString() + a})
    return result.joinToString("").trim()
}

private fun spaces(spaces: Int): String {
    if (spaces < 0)
        return ""
    return (1..spaces).map { ' ' }.joinToString("")
}