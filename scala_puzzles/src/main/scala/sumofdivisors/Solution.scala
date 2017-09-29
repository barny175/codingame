package sumofdivisors

import math._
import scala.util._

object Solution extends App {
  val n = 100

//  println((1 to n).map(divisors(_)).toList)
  println((1 to n).map(divisors(_).sum).toList.sum)

  def divisors(n: Int): List[Int] = {
//    println("n=" + n)
    def divNoOne(n: Int): List[Int] = {
      for (i <- 2 to n / 2) {
        if (n % i == 0) {
//          println(n / i)
          return i :: n :: divNoOne(n / i)
        }
      }
      List(n)
    }
    (1 :: divNoOne(n)).distinct
  }
}