package sumofdivisors

import math._
import scala.collection.mutable
import scala.util._

object Solution extends App {
  val n = 90000
  val divisorsMap: mutable.Map[Int, mutable.Set[Int]] = mutable.Map()

//  println((1 to n).map(divisors(_)).toList)
  println((1 to n).map(divisors(_).sum).map(i => i.toLong).toList.sum)

  def divisors(n: Int): mutable.Set[Int] = {
    val divs = mutable.Set[Int](1, n)
    var upper = n / 2
    var i = 2
    while (i <= upper) {
        if (n % i == 0) {
          val div = n / i
          if (divisorsMap.contains(div)) {
            divs ++= divisorsMap.get(div).get
          } else {
            divs ++= divisors(div)
          }
          divs += i
          upper = div
      }
      i += 1
    }
    divisorsMap.put(n, divs)

    divs
  }
}