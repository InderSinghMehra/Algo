object BfsDfs {
type Vertex=Int
type Graph=Map[Vertex,List[Vertex]]
val g:Graph=Map(1->List(2,5),2->List(1,5),3->List(4,6),4->List(2,3),5->List(1,2),6->List(3,5))
                                                  //> g  : BfsDfs.Graph = Map(5 -> List(1, 2), 1 -> List(2, 5), 6 -> List(3, 5), 2
                                                  //|  -> List(1, 5), 3 -> List(4, 6), 4 -> List(2, 3))
def BFS(start: Vertex, g: Graph): List[List[Vertex]]={
  val visited=List(start)
  val result=List(List(start))

  def BFS0(elems: List[Vertex],result: List[List[Vertex]], visited: List[Vertex]): List[List[Vertex]]={
    val newNeighbors=elems.flatMap(g(_)).filterNot(visited.contains).distinct
    if(newNeighbors.isEmpty) result
    else BFS0(newNeighbors, newNeighbors :: result, visited ++ newNeighbors)
    }

  BFS0(List(start),result,visited).reverse
   }                                              //> BFS: (start: BfsDfs.Vertex, g: BfsDfs.Graph)List[List[BfsDfs.Vertex]]

 def DFS(start: Vertex, g: Graph): List[Vertex]={
    var visited=List(start)
    var result=List(start)

  def DFS0(start: Vertex): Unit={
     for(n<-g(start); if !visited.contains(n)){
       visited=n :: visited
       result=n :: result
       DFS0(n)
  }}
   DFS0(start)
   result.reverse
}                                                 //> DFS: (start: BfsDfs.Vertex, g: BfsDfs.Graph)List[BfsDfs.Vertex]
BFS(1,g)                                          //> res0: List[List[BfsDfs.Vertex]] = List(List(1), List(2, 5))
BFS(2,g)                                          //> res1: List[List[BfsDfs.Vertex]] = List(List(2), List(1, 5))
DFS(3,g)                                          //> res2: List[BfsDfs.Vertex] = List(3, 4, 2, 1, 5, 6)
}