import java.util._

import java.lang._

import java.io._

import MST._

//remove if not needed
import scala.collection.JavaConversions._

object MST {

// Number of vertices in the graph
  private val V: Int = 5

  def main(args: Array[String]): Unit = {
    /* Let us create the following graph
		2 3
		(0)--(1)--(2)
 / \ |
		6| 8/ \5 |7
 /	 \ |
		(3)-------(4)
			9		 */

    val t: MST = new MST()
    val graph: Array[Array[Int]] = Array(Array(0, 2, 0, 6, 0),
                                         Array(2, 0, 3, 8, 5),
                                         Array(0, 3, 0, 0, 7),
                                         Array(6, 8, 0, 0, 9),
                                         Array(0, 5, 7, 9, 0))
// Print the solution
    t.primMST(graph)
  }

}

class MST {

// value, from the set of vertices not yet included in MST
  def minKey(key: Array[Int], mstSet: Array[java.lang.Boolean]): Int = {
// Initialize min value
    var min: Int = java.lang.Integer.MAX_VALUE
    var min_index: Int = -1
    for (v <- 0 until V if mstSet(v) == false && key(v) < min) {
      min = key(v)
      min_index = v
    }
    min_index
  }

// parent[]
  def printMST(parent: Array[Int], graph: Array[Array[Int]]): Unit = {
    println("Edge \tWeight")
    for (i <- 1 until V)
      println(parent(i) + " - " + i + "\t" + graph(i)(parent(i)))
  }

// using adjacency matrix representation
  def primMST(graph: Array[Array[Int]]): Unit = {
// Array to store constructed MST
    val parent: Array[Int] = Array.ofDim[Int](V)
// Key values used to pick minimum weight edge in cut
    val key: Array[Int] = Array.ofDim[Int](V)
// To represent set of vertices not yet included in MST
    val mstSet: Array[java.lang.Boolean] = Array.ofDim[Boolean](V)
    for (i <- 0 until V) {
      key(i) = java.lang.Integer.MAX_VALUE
      mstSet(i) = false
    }
// Make key 0 so that this vertex is
    key(0) = 0
// First node is always root of MST
    parent(0) = -1
    for (count <- 0 until V - 1) {
// not yet included in MST
      val u: Int = minKey(key, mstSet)
// Add the picked vertex to the MST Set
      mstSet(u) = true
      for (v <- 0 until V
           if graph(u)(v) != 0 && mstSet(v) == false && graph(u)(v) < key(v)) {
        parent(v) = u
        key(v) = graph(u)(v)
      }
    }
// Pick thd minimum key vertex from the set of vertices
// Update key value and parent index of the adjacent
// vertices of the picked vertex. Consider only those
// Pick thd minimum key vertex from the set of vertices
// Update key value and parent index of the adjacent
// vertices of the picked vertex. Consider only those
// print the constructed MST
    printMST(parent, graph)
  }
// Always include first 1st vertex in MST.
// picked as first vertex
// Always include first 1st vertex in MST.
// picked as first vertex

}
