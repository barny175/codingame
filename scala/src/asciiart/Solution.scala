
package asciiart

object Solution extends App {
//   val l = readInt
//    val h = readInt
//    val t = readLine
//    val letters : List[String] = (0 until h).map(_ => readLine).toList
    
    val l = 4
    val h = 5
    val t = "a@"
    val letters = List(" #  ##   ## ##  ### ###  ## # # ###  ## # # #   # # ###  #  ##   #  ##   ## ### # # # # # # # # # # ### ###",  
                       "# # # # #   # # #   #   #   # #  #    # # # #   ### # # # # # # # # # # #    #  # # # # # # # # # #   #   #", 
                       "### ##  #   # # ##  ##  # # ###  #    # ##  #   ### # # # # ##  # # ##   #   #  # # # # ###  #   #   #   ##",
                       "# # # # #   # # #   #   # # # #  #  # # # # #   # # # # # # #    ## # #   #  #  # # # # ### # #  #  #      ",       
                         "# # ##   ## ##  ### #    ## # # ###  #  # # ### # # # #  #  #     # # # ##   #  ###  #  # # # #  #  ###  # ")

    
    val ascii = t.map(letterToAscii(_))
                 .foldLeft((0 until h).map(_ => "").toList)(accFun)
    
    ascii.foreach { x => println(x) }
   
    def accFun(acc: List[String], l: List[String]) : List[String] = {
        acc.zip(l).map(t => t._1 + t._2)
    }
    
    def letterToAscii(letter:Char) : List[String] = {
        val i = letter.toUpper - 'A'
        if (i < 0 || i >= letters.size)
            letters.map(a => a.substring(a.length - l, a.length))
        else
            letters.map(_.substring(i * l, (i + 1) * l))
    }
}