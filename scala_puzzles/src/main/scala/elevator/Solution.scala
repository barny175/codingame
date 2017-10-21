import math._
import scala.util._


object Solution extends App {
  //  val Array(floors, up, down, start, target) = for(i <- readLine split " ") yield i.toInt
  val Array(floors, up, down, start, target) = Array[Int](11, 2, 3, 10, 6)
//  val Array(floors, up, down, start, target) = Array[Int](10, 1, 1, 8, 5)

  val steps = nextMove(List(start), Set(), 0)
  println(steps.map(i => i.toString)
      .getOrElse("IMPOSSIBLE"))

  def nextMove(positions: List[Int], visited: Set[Int], steps: Int): Option[Int] = {
    val newPositions = positions
      .flatMap(p => List(p - down, p + up))
      .filter(p => p >= 0 && p <= floors)
      .filter(p => !visited.contains(p))

    val newVisited = visited ++ newPositions
    if (newVisited.contains(target))
      Option(steps + 1)
    else if (newPositions.isEmpty)
      Option.empty
    else
      nextMove(newPositions, newVisited, steps + 1)
  }
}