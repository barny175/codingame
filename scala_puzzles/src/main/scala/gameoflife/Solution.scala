package gameoflife


import math._
import scala.util._

object Solution extends App {
  val Array(width, height) = for(i <- readLine split " ") yield i.toInt
  val board: Array[Array[Int]] = new Array(height)
  for(i <- 0 until height) {
    val line:Array[Int] = readLine().chars().toArray()
    board(i) = line
  }

  for (r <- 0 until height) {
    for (c <- 0 until width){
      val n = neighbours(board, r, c)
      //   println(s"$r $c $n")
      if (n < 2 || n > 3)
        print('0')
      else {
        if (n == 3)
          print('1')
        else
          print(board(r)(c).toChar)
      }
    }
    println()
  }

  def neighbours(board: Array[Array[Int]], row: Int, col: Int): Int = {
    var live: Int = 0
    for (r <- (row - 1) to (row + 1); c <- (col - 1) to (col + 1)) {
      if (r >= 0 && r < board.length) {
        if (c >= 0 && c < board(r).length) {
          if ((r != row || c != col) && board(r)(c) == '1')
            live += 1
        }
      }
    }
    return live
  }
}