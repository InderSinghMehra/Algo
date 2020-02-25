import scala.io.Source
import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set

class Edge
{
    var a: Char = '0'
    var b: Char = '0'
    var weight: Int = 0
    def this(a: Char, b: Char, weight: Int)
    {
        this()
        this.a = a
        this.b = b
        this.weight = weight
    }
    override def toString(): String = 
    {
        return a.toString + "--" + b.toString + ", w = " + weight.toString
    }
}

class Graph
{
    var vertices = new ArrayBuffer[Char]()
    var edges = new ArrayBuffer[Edge]()
    override def toString(): String = 
    {
        var result: String = ""
        result = "Vertices:\n"
        for (i <- 0 to vertices.length-1)
        {
            result = result + vertices(i) + ", "
        }
        result = result + "\n"
        result = result + "Edges:\n"
        for (i <- 0 to edges.length - 1)
        {
            result = result + edges(i).toString + "\n"
        }
        return result
    }
}

class Kruskal
{
    val graph = new Graph()

    def this(fileName: String)
    {
        this()
        val source = Source.fromFile(new File(fileName))
        for (line <- source.getLines)
        {
            val tempArray = line.split(" ")
            graph.vertices.append(tempArray(0)(0))
            val numberOfAdjacentEdges = (tempArray.length - 1)/2
            for (i <- 0 to numberOfAdjacentEdges-1)
            {
                graph.edges.append(new Edge(tempArray(0)(0), tempArray(1+i)(0), (tempArray(numberOfAdjacentEdges+1+i).filter(_ != ',')).toInt))
            }
        }
        source.close()
    }

    def quickSortEdges(edges: ArrayBuffer[Edge]): ArrayBuffer[Edge] = 
    {
        val length: Int = edges.length
        if (length == 0 || length == 1)
        {
            return edges
        }
        var lower: ArrayBuffer[Edge] = new ArrayBuffer[Edge]()
        var equal: ArrayBuffer[Edge] = new ArrayBuffer[Edge]()
        var upper: ArrayBuffer[Edge] = new ArrayBuffer[Edge]()
        val pivot: Int = edges(0).weight
        for (i <- 0 to length - 1)
        {
            if (edges(i).weight < pivot)
                lower.append(edges(i))
            else if (edges(i).weight == pivot)
                equal.append(edges(i))
            else
                upper.append(edges(i))
        }
        lower = quickSortEdges(lower)
        upper = quickSortEdges(upper)
        return lower ++ equal ++ upper
    }
    def sortEdges()
    {
        graph.edges = quickSortEdges(graph.edges)
    }

    def minimumSpanningTree(): ArrayBuffer[Edge] = 
    {
        var result: ArrayBuffer[Edge] = new ArrayBuffer[Edge]()
        var sets: ArrayBuffer[Set[Char]] = new ArrayBuffer[Set[Char]]()
        
        def findSet(c: Char, sets: ArrayBuffer[Set[Char]]): Int = 
        {
            val len = sets.length
            for (i <- 0 to len-1)
            {
                if (sets(i).contains(c))
                    return i
            }
            return -1
        }
        sortEdges()
        for (v <- graph.vertices)
        {
            sets.append(Set[Char](v))
        }
        for (edge <- graph.edges)
        {
            var index_a = findSet(edge.a, sets)
            var index_b = findSet(edge.b, sets)
            if (index_a != index_b) 
            {
                result.append(edge)
                sets(index_a) = sets(index_a) ++ sets(index_b)
                sets(index_b) = sets(index_a)
            }
        }
        return result
    }

    def printGraph(): Unit = 
    {
        print(graph.toString)
    }
}

object Kruskal
{
    def main(args: Array[String]): Unit = 
    {
        val kruskal = new Kruskal("graph.txt")
        kruskal.sortEdges()
        kruskal.printGraph()
        val tree:ArrayBuffer[Edge] = kruskal.minimumSpanningTree()
        println("Tree: ")
        for (t <- tree)
        {
            println(t)
        }
    }
}
