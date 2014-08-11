import akka.routing.Broadcast
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import java.io._
import akka.actor.{Actor, Props, PoisonPill, Terminated}
import Actor._
import akka.actor.ActorSystem
import akka.actor.ActorRef
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import Array._
import scala.io.Source._
import scala.io.Source

object router {

  trait project
  
  //message to master actor 
  
  // send by main method to generate graph from input file
  case class fill_graph(fileName: String, sourceVertex: Int, size: Int) extends project 
  
  // send by wroker actors to notify that minimum is found
  case class minimum_found(min: Array[Int]) extends project

  // send by self to compute global_minimum after all the local minimum are done
  case class find_global(minlist: Array[Array[Int]]) extends project
  
  // worker actors send their local cost[] array to master
  case class send_cost(cost: Array[Int], firstindex: Int, lastindex: Int) extends project

  // After workers are done with their job, they ask for more work if available
  case class update_test extends project
  
  //message to workers

  // send by master actor to compute local minimum
  case class find_local_minimum extends project

  // send my master to update the cost[] with the global minimum found
  case class update_path(globalmin: Array[Int]) extends project

  // After all the shortest path are found for the vertices, master requests for the local cost[]
  case class request_own_cost extends project

  // to initialize some of the parameters for each worker                         
  case class init(id:Int) extends project

  
  // This class acts as the worker actor to carry out the work independently for a given index
  //  of the matrix.This is instantiated in the master actor using router. 

  class Worker(size: Int, totSize: Int, graph: Array[Array[Int]], sourceVertex: Int) 
		  	   extends Actor {

    var idw = 0                      // id of the actor
    var firstIndex = 0               // first index of the graph that this actor would work on
    var totalSize = totSize 
    var lastIndex =  0               // last index of the graph that this actor would work on
    val status = Array.fill(totalSize)(false)      // to keep status of which nodes are visited
    val cost = Array.fill(totalSize)(Int.MaxValue) // cost[] to keep cost of reaching a vertex
    val localGraph = graph                         // graph to hold the graph
    var localMin = new Array[Int](2)       // to hold the local minimum value and its index
    var globalMin = new Array[Int](2)      // to hold the value of global minimum and its index
    var sizeMatrix = size                  // size of the matrix 
    
    def receive = {
      
      case init(id) =>
        idw = id
        firstIndex = id * size
        lastIndex = firstIndex + size - 1
        if (id == 0)  cost(sourceVertex) = 0
      
      case `find_local_minimum` =>
        
        localMinimum
        sender ! minimum_found(localMin)
      
      case update_path(gmin) =>
        
        globalMin = gmin
        updateCost(globalMin)
        status(globalMin(1)) = true
        sender ! update_test
      
      case `request_own_cost` =>

        sender ! send_cost(cost, firstIndex, lastIndex)
        
        
    }

    // method to update the cost[] using the global minimum. This method first checks 
    //  if there is any edge or not between the two verticesand if that vertex is 
    //  visited or not. IF not, it calculates its distance from the minvertex and checks
    // if the distance  from source is greater than the distance from source to
    //  min vertex + from min vertex to the vertex and updates the path accordingly.

    def updateCost(globalMin: Array[Int]): Unit = {

      var minDistance = globalMin(0)
      var minVertex = globalMin(1)
      var temp = 0

      for (i <- firstIndex until lastIndex + 1) {
    	   temp = graph(i)(minVertex)
    	
         if (temp > 0 && !status(i)) {
           if (cost(i) > temp + minDistance) {
             cost(i) = temp + minDistance
            
           }
         }
       }
      
     }
    
    // method to find local minimum. It simply checks which is the minimum among 
    //all the vertices in the entire cost[] by traversing the cost[] once.

    def localMinimum(): Unit = {
      
      var i = firstIndex
      var min = Int.MaxValue
      localMin(0) = Int.MaxValue
      while (i <= lastIndex) {
        if (!status(i))	{
          if (min >= cost(i)) {
            min = cost(i)
            localMin(0) = min		// for value
            localMin(1) = i		// for index
          }
        }
        i += 1
      }
      
    }
    
  }
  
  
  class Master(numActors: Int) extends Actor {
    
    val actorCount = numActors          // number of actors used
    var size = 0                        
    var graph = ofDim[Int](size, size)    // graph as 2d array
    var vertexCount = 0
    var msgCount = 0			// to count the no. of mins received
    var costCount = 0			// to count the no. of cost received
    var globalMin = Array[Int](2)             // to store the global minimum
    var minlist = ofDim[Int](actorCount, 2)  // to store the minimum of each actor 
    var totalCost = new Array[Int](size)     // stores the final cost for each vetex
    var sourceVertex = 0                     // source vertex from which to start the algorithm
    var startTime = 0.0
    var router = context.actorOf(Props(new Worker(size/actorCount, size, graph, 0)).withRouter(
                        RoundRobinRouter(nrOfInstances = actorCount)
                     ), name = "simplerouter")
    def receive = {
      
      case fill_graph(filename, source, siz) =>

        size = fillGraph(filename, siz)             // read the file  and make the graph
        sourceVertex = source
        
        // router takes care of routing and broadcasting messages to the worker actors. 
	// After master creates the router actor, it can send message
        // directly to the router, and router will pass or broadcast the same message to all the 
	// routees(the actors creates inside router).
        // when workers want to send message back to the master 
	// i.e. the parent they can directly use sender ! msg.

        router = context.actorOf(Props(new Worker(size/actorCount, size, graph, source)).
				withRouter(RoundRobinRouter(nrOfInstances = actorCount)
		                     ), name = "simplerouter2")
        for (i <- 0 until actorCount) {
          router ! init(i)                            // initialize all the actors
        }
        startTime = System.currentTimeMillis()
        router ! Broadcast(find_local_minimum)       // broadcast to all the workrers
        
      case minimum_found(localMin) =>
        
	// keep count of minimum received from the workers. If all workers have sent their minimum

        msgCount += 1                                     
        minlist(msgCount - 1)(0) = localMin(0)                
        minlist(msgCount - 1)(1) = localMin(1)
        if (msgCount == actorCount) {
        	self ! find_global(minlist)	// then find global minimum among them
        	msgCount = 0
        }
        	
      case find_global(minlist) =>

	// finds global minimum and broadcast to all workers to update their cost[] array.

        globalMin = findGlobal(minlist)
        router ! Broadcast(update_path(globalMin))
        
      case `update_test` =>

        vertexCount += 1
        
	// if all the vertices are relaxed, ask each actor for their cost array
	// if not, ask the workers to calculate the local minimum again

        if (vertexCount == (size) * actorCount) {
            router ! Broadcast(request_own_cost)
        }
        else {
          sender ! find_local_minimum
        }
       
      case send_cost(cost, firstindex, lastindex) =>

	// receives the cost from all the workers combine it and write to a file

        for (i <- firstindex until lastindex + 1)
        	totalCost(i) = cost(i)
	
        costCount += 1
        if (costCount == actorCount) {    
          println("Total time taken is " + (System.currentTimeMillis() - startTime))
          writeCostToFile(totalCost, sourceVertex)

	  // Now work is finished, kill all the actors and self and exit
          router ! Broadcast(PoisonPill)
          self ! PoisonPill
          sys.exit
        }
        
    }

    // this method writes the final cost array to the file in format:
    // sourceVertex      vertex     cost
    
    def writeCostToFile(totalCost: Array[Int], sourceVertex: Int): Unit = {
      
      val writer = new PrintWriter(new File("cost.log" ))     		   // make a new file
      var i = 0
      for (x <- 0 until totalCost.length) {
        writer.write(sourceVertex + " " + i + " " + totalCost(x) + "\n")  // write to the file
        i += 1
      }
      writer.close()                  					  // close the file
    }

    // method to find the global minimum among the local minimum. 
    // It compares which is minimum among all and saves the global minimum and its index
    
    def findGlobal(minlist: Array[Array[Int]]): Array[Int] = {

      var i = 0
      var min = Int.MaxValue
      var index = 0
      var global = new Array[Int](2)
      while (i < minlist.length) {
        if (min > minlist(i)(0)) {
          min = minlist(i)(0)
          index = minlist(i)(1)
        }
        i += 1
      }
      global(0) = min
      global(1) = index
      
      global
    }
    
    // method to read the input file and save it to a 2d array.


    def fillGraph (filename: String, siz: Int): Int = {
      val size = siz
      graph = ofDim[Int](size, size)
      totalCost = new Array[Int](size)
      for (line <- Source.fromFile(filename).getLines) {
        val pair = line.split(" ")
        val v1 = pair(0).toInt
        val v2 = pair(1).toInt
        val v3 = pair(2).toInt
        graph(v2)(v1) = v3
      }
      
      size
    }
    
  }
  
  // main method that starts the whole program

  def main(args: Array[String]) {

    var numActors = 4
    var filename = args(0)    // take filename is input
    var sourceVertex = 0
    var size = args(1).toInt  // take size as input
    val system = ActorSystem("Dijkstra")
    val master = system.actorOf(Props(new Master(numActors)), "master")
    
    master ! fill_graph(filename, sourceVertex, size)
    
  }
}
