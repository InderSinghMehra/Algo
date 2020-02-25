import java.util.ArrayList
import java.util.Arrays
import java.util.List
import scala.collection.JavaConversions._
class Edge(var source: Int, var dest: Int)
class Graph // Constructor
(edges: List[Edge], N: Int) {
// An array of Lists to represent adjacency list
  var adjList: List[List[Integer]] = new ArrayList(N)
  for (i <- 0 until N) {
    adjList.add(i, new ArrayList())
  }
  for (i <- 0 until edges.size) {
    val src: Int = edges.get(i).source
    val dest: Int = edges.get(i).dest
    adjList.get(src).add(dest)
    adjList.get(dest).add(src)
  }
}
object HamiltonianPaths {
  def printAllHamiltonianPaths(g: Graph,
                               v: Int,
                               visited: Array[Boolean],
                               path: List[Integer],
                               N: Int): Unit = {
// hamiltonian path exists
    if (path.size == N) {
// print hamiltonian path
      for (i <- path) System.out.print(i + " ")
      println()
      return
    }
// to a solution or not
    for (w <- g.adjList.get(v) if !visited(w)) {
      visited(w) = true
      path.add(w)
// to solution or not
      printAllHamiltonianPaths(g, w, visited, path, N)
// Backtrack
      visited(w) = false
      path.remove(path.size - 1)
    }
// check if adding vertex w to the path leads
// check if adding vertex w to the path leads
  }
// if all the vertices are visited, then
// Check if every edge starting from vertex v leads
// if all the vertices are visited, then
// Check if every edge starting from vertex v leads
  def main(args: Array[String]): Unit = {
// List of graph edges as per above diagram
    val edges: List[Edge] = Arrays.asList(new Edge(0, 1),
                                          new Edge(0, 2),
                                          new Edge(0, 3),
                                          new Edge(0, 4),
                                          new Edge(1, 2),
                                          new Edge(1, 3),
                                          new Edge(1, 4),
                                          new Edge(2, 3),
                                          new Edge(2, 4),
                                          new Edge(3, 4))
// Set number of vertices in the graph
    val N: Int = 5
// create a graph from edges
    val g: Graph = new Graph(edges, N)
// starting node
    val start: Int = 0
// add starting node to the path
    
    val path: List[Integer] = new ArrayList[Integer]()
    path.add(start)
// mark start node as visited
    val visited: Array[Boolean] = Array.ofDim[Boolean](N)
    visited(start) = true
    printAllHamiltonianPaths(g, start, visited, path, N)
  }
}
