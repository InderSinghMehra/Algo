import java.util.Arrays
import java.util.LinkedList
import java.util.Scanner
import java.lang.System.out
//remove if not needed
import scala.collection.JavaConversions._
object Bellman_Ford_Algorithm {

  var total_nodes: Int = _

  var edges: Int = _

  var Edges: LinkedList[Edge] = _

  def bellmanFord(source: Int): Array[Int] = {
//initialise distance array
    val distances: Array[Int] = Array.ofDim[Int](total_nodes + 1)
//set all distances node maximum as possible
    Arrays.fill(distances, java.lang.Integer.MAX_VALUE)
//set source distance 0
    distances(source) = 0
    for (i <- 0 until Edges.size
         if distances(Edges.get(i).x) != java.lang.Integer.MAX_VALUE
         if distances(Edges.get(i).x) + Edges.get(i).weight < distances(
           Edges.get(i).y))
      distances(Edges.get(i).y) = distances(Edges.get(i).x) + Edges.get(i).weight//return distances array
    distances
  }

  def main(as: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
//Process Inputs
    total_nodes = sc.nextInt()
    edges = sc.nextInt()
//Initialise Edges
    Edges = new LinkedList()
//Add Edge :   from -> next = weight
    while ({ edges -= 1; edges + 1 } > 0) Edges.add(
      new Edge(sc.nextInt(), sc.nextInt(), sc.nextInt()))
//print distance array
    out.println(Arrays.toString(bellmanFord(sc.nextInt())))
  }

}

class Edge(var x: Int, var y: Int, var weight: Int)
