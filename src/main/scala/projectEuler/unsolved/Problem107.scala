//package projectEuler.unsolved
//
//import projectEuler.utils.Problem
//
//class Problem107 extends Problem{
//
//  def runAlgo(graph: List[Edge]): Int ={
//    val allEdges = graph.sortBy(_.length)
//    var optimized = allEdges.toVector
//    for (edge <- allEdges.sorted) {
//      if (connectedWithoutThisEdge(edge, optimized)) optimized = optimized filter (_ == edge)
//    }
//    optimized.reduce(_.length + _.length)  // change to map-reduce (or it is called foldmap)
//  }
//
//  def distanceTo(v1: Int, v2: Int, edges: Vector[Edge]): Int = {
//    ???
//  }
//
//  private def connectedWithoutThisEdge(edge: Edge, graph: Vector[Edge]): Boolean = {
//    distanceTo(edge.v1, edge.v2, graph.filter(_ == edge)) > 0
//  }
//
//  def readGraph() = ???
//
//  override def solve(): Unit = {
//    val graph = readGraph()
//    runAlgo(graph)
//  }
//
//  case class Edge(v1: Int, v2: Int, length: Int)
//}
