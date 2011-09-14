import java.io.LineNumberReader;
import java.io.FileReader;
import java.io.File;
import scala.collection.mutable.Map;
import scala.collection.mutable.Set;
import scala.collection.immutable.TreeSet;

val graph = Map[Int, Set[Int]]()

def numToChar(num:Int) = (num + 'A').toChar

def convert(node:Int) : String =
  if (node < 26) "" + numToChar(node % 26)
  else convert(node / 26 -1) + numToChar(node % 26)

def addToGraph(from:Int, to:Int) =
{
  if (!graph.contains(from)) graph(from) = Set.empty[Int];
  graph(from) += to;
}

val lines = scala.io.Source.fromFile(args(0)).getLines.toList;

for (line <- lines if !line.startsWith("#"))
{
  val nodes = line.split(" ");
  val mainNode = nodes(0).toInt;
  for (node <- nodes;
       nodeNum = node.toInt;
       if nodeNum != mainNode)
  {
    addToGraph(mainNode, nodeNum);
    addToGraph(nodeNum, mainNode);
  }
}
println(graph.size);
val nodes = TreeSet[Int]() ++ graph.keySet;
for (node <- nodes)
{
  val adjNodes = TreeSet[Int]() ++ graph(node);
  print(convert(node));
  print(":");
  println(adjNodes.map(convert).mkString(","));
}
