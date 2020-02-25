object dfs {

  type Vertex=Int
  type Graph=Map[Vertex,List[Vertex]]
  val g: Graph=Map(1 -> List(2,4), 2-> List(1,3,5), 3-> List(2,4), 4-> List(1,3),5-> List(2,6),6->List(3,5))
                                                  //> g  : dfs.Graph = Map(5 -> List(2, 6), 1 -> List(2, 4), 6 -> List(3, 5), 2 ->
                                                  //|  List(1, 3, 5), 3 -> List(2, 4), 4 -> List(1, 3))
  def DFS(start: Vertex, g: Graph): List[Vertex] = {

  def DFS0(v: Vertex, visited: List[Vertex]): List[Vertex] = {
    if (visited.contains(v))
      visited
    else {
      val neighbours:List[Vertex] = g(v) filterNot visited.contains
      neighbours.foldLeft(v :: visited)((b,a) => DFS0(a,b))
    }
  }
  DFS0(start,List()).reverse
 }                                                //> DFS: (start: dfs.Vertex, g: dfs.Graph)List[dfs.Vertex]
             DFS(1,g)                             //> res0: List[dfs.Vertex] = List(1, 2, 3, 4, 5, 6)

}