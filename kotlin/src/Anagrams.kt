import java.util.*

fun main(args: Array<String>) {
    val input = Scanner(System.`in`)
    val phrase = input.nextLine()

    val step34 = step(step4(phrase), ::step3transformation, { c: Char -> (c - 'A' + 1) % 4 == 0 })
    System.err.println(step34)
    val step2 = step(step34, ::step2transformation, { c: Char -> (c - 'A' + 1) % 3 == 0 })
    val step1 = step(step2, { l -> l.asReversed() }, { c -> (c - 'A' + 1) % 2 == 0 } )
    println(step1)
}


fun step2transformation(list: List<Int>): List<Int> {
    val res = mutableListOf<Int>(list.last())
    res.addAll(list.take(list.size - 1))
    return res;
}

fun step3transformation(list: List<Int>): List<Int> {
    val res = mutableListOf<Int>()
    res.addAll(0, list.drop(1))
    res.add(list.first())
    return res;
}

fun step(input: String, transformation: (List<Int>) -> List<Int>, pred: (c: Char) -> Boolean): String {
    val indexes = input.indices.filter { input[it] > 'A' && pred(input[it]) }
    if (indexes.isEmpty())
        return input

    val shifted = transformation(indexes)
    System.err.println(indexes)
    System.err.println(shifted)

    return input.mapIndexed { ind, c ->
        if (shifted.contains(ind))
            input[indexes[shifted.indexOf(ind)]]
        else
            c
    }.joinToString("")
}

fun step4(phrase: String): String {
    val split: List<String> = phrase.split(" ");
    val lengths = split.map { s -> s.length };
    val reversed = lengths.asReversed()
    var result: List<String> = listOf()
    val noSpaces = split.joinToString("")
    var drop = 0;
    for (len in reversed) {
        result += noSpaces.substring(drop, drop + len)
        drop += len;
    }
    return result.joinToString(" ")
}