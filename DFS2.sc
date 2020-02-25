object DFS2 {
  type Vertex=Int
type Graph=Map[Vertex,List[Vertex]]
val g:Graph=Map(1->List(2,5),2->List(1,5),3->List(4,6),4->List(2,3),5->List(1,2),6->List(3,5))
                                                  //> g  : DFS2.Graph = Map(5 -> List(1, 2), 1 -> List(2, 5), 6 -> List(3, 5), 2 -
                                                  //| > List(1, 5), 3 -> List(4, 6), 4 -> List(2, 3))
def DFS(node: Vertex,graph: Graph):List[Vertex]= {
  var visited = List(node)
  var result = List(node)
  def DFS1(node: Vertex): Unit = {
    for (n <- g(node); if !visited.contains(n)) {
      visited = n :: visited
      result = n :: result
      DFS1(n)
    }
  }
  DFS1(node)
  result.reverse
}                                                 //> DFS: (node: DFS2.Vertex, graph: DFS2.Graph)List[DFS2.Vertex]
DFS(3,g)                                          //> res0: List[DFS2.Vertex] = List(3, 4, 2, 1, 5, 6)
}