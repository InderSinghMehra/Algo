import collection.mutable
object Graph
{
    case class Edge(val dest: Vertex, val weight:Int)
    class Vertex {
      val edges = scala.collection.mutable.Buffer[Edge] ()
    }
    val graph1 = Array.fill(4) (new Vertex)
    graph1(0).edges += Edge(graph1(1),2) += Edge(graph1(2),3)
    val graph2 = Array(Array(0,2,3,0),
                       Array(0,0,5,4),
                       Array(0,0,0,2),
                       Array(1,0,0,0))
      def canReach(cur:Int,dest:Int,graph:Array[Array[Int]]):Boolean = {
      if(cur==dest) true
      else {
        var ret = false
        for(i <-graph(cur).indices) {
          if(graph(cur)(i) > 0) {
            ret = ret || canReach(i,dest,graph)
          }
        }
      ret
      
     //alternative
     //  graph(cur).indices.exists(i => graph(cur)(i)>0 && canReach(i,dest,graph))
      }
    }
   def main(args:Array[String]) {
    println(canReach(0,3,graph2))
       }
}