import java.util.*
import java.io.*
import java.math.*


val cache: MutableMap<Long, Long> = HashMap()

fun main(args : Array<String>) {
    val input = Scanner(System.`in`)
    val start = input.nextLong()
    val n = input.nextLong()

    var res = start
    for (i in 1..n)
        res = toBin(res)

    println(res)
}

fun toBin(n: Long): Long {
    val cached = cache.get(n)
    if (cached != null)
        return cached

    var i = n
    var result: Long = 0
    while (i > 0) {
        if (i % 2 == 0L)
            result += 4
        else
            result += 3
        i = i / 2
    }
    cache.put(n, result)
    return result
}
