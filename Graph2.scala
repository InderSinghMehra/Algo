import collection.mutable
object Graph2 extends App {
  case class Edge(val dest: Vertex, val weight: Int)
  class Vertex {
    val edges = mutable.Buffer[Edge]()
  }
  val graph1 = Array.fill(4)(new Vertex)
  graph1(0).edges += Edge(graph1(1), 2) += Edge(graph1(2), 3)
  val graph2 = Array(
    Array(0, 0, 3, 0),
    Array(0, 0, 5, 4),
    Array(0, 0, 0, 2),
    Array(1, 0, 0, 0))

  def canReach(cur: Int, dest: Int, graph: Array[Array[Int]], visited: Array[Boolean]): Boolean = {
    visited(cur) = true
    println(cur)
    if (cur == dest) true
    else {
      var ret = false
      for (i <- graph(cur).indices) {
        if (!visited(i) && graph(cur)(i) > 0) {
          ret = ret || canReach(i, dest, graph2, visited)
        }
      }
      ret

    }
  }

  def shortestPath(cur: Int, dest: Int, graph: Array[Array[Int]], visited: Set[Int]): Int = {
    val newvisited = visited + cur
    println(cur)
    if (cur == dest) 0
    else {
      val pathLength = for (i <- graph(cur).indices; if (!visited(i) && graph(cur)(i) > 0)) yield {
        graph(cur)(i) + shortestPath(i, dest, graph2, newvisited)
      }
      pathLength.min

    }
  }
  println("if it can reach")
  println(canReach(0, 1, graph2, Array.fill(graph2.size)(false)))
  println("shortest path from a to b")
  println(shortestPath(0, 3, graph2, Set()))

}