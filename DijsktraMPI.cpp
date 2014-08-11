#include <iostream>
#include <vector>
#include <mpi.h>
#include <set>
#include <cmath>
#include <cstdlib>
#include <climits>
#include <fstream>
#include <cstring>
using namespace std;
 
/*
* Structure to store the local and global Minimum weight and corresponding vertex number
*/
typedef struct
{
    int value;
    int index;
}minNode;
 

/* Using dfs after creation of graph to check if it is connected or not
* @param adjMat : 2D matrix in which the graph is stored
* @param visited : A vector to store the status of vertices, whether they are visited or not 
* @param u : starting vertex at which to begin
* @param vertices : no of vertices in the graph
* @void : NULL
*/

void dfs(int **adjMat, vector<bool> &visited, int u, int vertices)
{
    visited[u] = true;
    int v;
    for(int i =0; i< vertices; i++)
    {  
        if(adjMat[u][i] > 0)
        {  
            v = i;           
            if(!visited[v])
                dfs(adjMat,visited,v, vertices);
        }
    }
}

/* Using check_connected after creation of graph to check if it is connected or not
* @param adjMat : 2D matrix in which the graph is stored
* @param vertices : no of vertices in the graph
* @return bool : true if the graph is connected, false otherwise
*/
         
bool check_connected(int **adjMat, int vertices)
{
    vector<bool> visited;
 
    for(int j =0; j < vertices; j++)
        visited.push_back(bool());
 
     dfs(adjMat, visited, 0, vertices);
 
    for(int j =0; j < vertices; j++)
    {
        if(visited[j] == false)
            return false;
    }
 
    return true;
 
}

/* Using this function to generate a random graph
* @param adjMat : 2D matrix in which the graph is stored
* @param vertices : no of vertices in the graph
* @param density : The density of the graph
* @return bool : true if connected graph is created, false otherwise
*/
bool createRandomGraph(int **adjMat, int vertices, int density)
{
    int i =0, number = 0;
    int vertex1, vertex2, weight ,edge_count = 0;
    srand(time(0));
    float d = (float)density/100;
    long long final_edge_count = d*vertices*(vertices - 1)/2;
    if (final_edge_count < (vertices-1))    			// checking for configuration for 
								//which connected graph is not 
								//possible
    {  
        cout<<"connected graph not possible for this value of vertices and density" <<endl;
        return false;
    }
    set<int>  :: iterator it1;
    vector<set<int> > edges;				//storing edges, so that we dont try 
							//to store the same edge again.
    for(int i = 0; i < vertices; i++)
        edges.push_back(set<int>());
 
    bool already_inserted ;
    do
    {
        for(int j =0; j < vertices; j++)
        {
            edges[j].clear();
        }
        edge_count = 0;
         
        do
        {
            number++;
            vertex1 = rand()%vertices;
            vertex2 = rand()%vertices;
            weight = (rand()%1001) + 20;
            it1 = edges[vertex1].find(vertex2);
            already_inserted = false;
             
            if(it1 != edges[vertex1].end())
            {
                if(*it1  == vertex2)
                    already_inserted = true;
            }
 
 
            							//Adding edge to the Adjacency 
								// matrix if the edge has not 
								//already been added.
            if((already_inserted == false) && (vertex1 != vertex2)) 
            {
                edges[vertex1].insert(vertex2);
                adjMat[vertex2][vertex1] = weight;
                edge_count++;
            }
        }while(final_edge_count != edge_count);
    }while(!check_connected(adjMat,vertices));		//checking if the graph is connected 
							//or not
 
 
 
   								//Writing the created Adjacency 
								//matrix to a file for future use
    ofstream ofile;
    char filename[50];
    sprintf(filename,"graph%d_%d", vertices, density);
    ofile.open(filename);
    for(int i = 0; i < vertices; i++)
    {  
        for(int j = 0; j < vertices; j++)
                ofile<<i<<" "<<j<<" "<<adjMat[j][i]<<endl;
    }
    ofile.close();
    return true;
 
}
 
/* Utility funtion to read graph from a file
*  NOTE : We are storing inlinks of a graph in this adjaceny matrix
*  The Scatter function in MPI scatters contiguous blocks of memory to processors
*  Keeping inlinks helps us scatter the 2D Matrix to processes easier.
*  @param adjMat : 2D matrix in which the graph is stored
*  @param filename : File from which the graph has to be read
*/
void readFile(int **adjMat, char* filename)
 
{
    ifstream infile;
    infile.open(filename);
    int i, j, weight;
 
    while(infile>>i>>j>>weight)
        adjMat[j][i] = weight; 
 
}
 
 
 
/*Utility functions to create a dynamically sized 2d array on the heap
* @param rows : row size of matrix
* @param columns : column size of matrix
* @return : void
*/ 	
int ** allocate2DArray(int rows, int columns)
{
    int * array = new int[rows*columns];  	// Allocating continous memory
    int **data = new int*[rows];
    for(int i = 0; i < rows; i++)
        data[i] = &(array[i*columns]);    	// putting two ints in each row of the 2d array.
    return data;	
 
}
 
int main(int argc, char * argv[])
{  
 
     
    int my_id, numprocs;
    char* filename = argv[3];			//uncomment this line we you want to read from file
    MPI_Init(NULL, NULL) ;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_id) ;
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs) ;
    int vertexCount = atoi(argv[1]);
    int density =  atoi(argv[2]); 
    int **adjMat = NULL;
    int *localDistances = NULL;
    bool * visited = NULL;
    int **localAdjMat;
    int localVertexCount;
    int *sendcount;
    int *displ;
    int i;
    int lastLocalVertexCount;
    int visitedCount;
    int localVisitedCount = 0;
    int minValue;
 
     
    localVertexCount = vertexCount/numprocs; //Calculate amount of work given to each processor
 
    lastLocalVertexCount = vertexCount - (numprocs - 1)*localVertexCount;//The work might not be
									// a multiple of no of
									// processors, hence the
									// last processor can
									// get more work
 
 
    if(my_id == 0)
    {
        adjMat = allocate2DArray(vertexCount,vertexCount);		//Creating 2D matrix to 
									//store the graph on the
									// heap. This is done 
									//only on process 0.
        for(int i = 0; i < vertexCount; i++)
            memset(adjMat[i], 0 , sizeof(int)*vertexCount);		//Initializing the graph
									// with 0's

        createRandomGraph(adjMat,vertexCount,density);		//Based on user Input, creating
								// random graph
	//readFile(adjMat, filename);				//uncomment this line, if you
								// want to read from file and 
								//give filename input in 
								//commandline as third argument
       
	 sendcount = new int[numprocs];				//Arrays used in the MPI_Scatterv 
								//function for variable size 
								//work distribution.
        displ = new int[numprocs];
 
        for(int i = 0; i < numprocs; i++)
        {
            sendcount[i] = localVertexCount*vertexCount;   
            displ[i] = i*localVertexCount*vertexCount;
        }      
     
       										
        sendcount[numprocs-1] = lastLocalVertexCount*vertexCount;	//Giving different sized
									// partition to the last
									// processor  if size is
									// not multiple of 
									//numprocs
         
    }
    else
        adjMat = new int*[1];					//if my_id is not 0, still 
								//initialzing adjmat with a int 
								//pointer.We do this to make the
								// Scatterv c code correct for
								// all the processors.
 
    if(my_id != numprocs - 1)					//creating local adjacency matrix
								// at each processor
    {
        localAdjMat = allocate2DArray(localVertexCount, vertexCount);		
    }
    else
    {
        localAdjMat = allocate2DArray(lastLocalVertexCount, vertexCount);      
        localVertexCount = lastLocalVertexCount;
    }
     
    MPI_Scatterv(*adjMat, sendcount, displ, MPI_INT, *localAdjMat, localVertexCount*vertexCount, 
		MPI_INT, 0, MPI_COMM_WORLD);
  
    if(my_id == 0)
    {
        delete []*adjMat;
        delete []adjMat;
    }
 
 
    double start = MPI_Wtime();
    localDistances = new int[localVertexCount];			//creating distance vector at 
								//each processor to store 
								//shortest path distances.
 
    for(int i = 0 ; i < localVertexCount; i++)
        localDistances[i] = INT_MAX;
 
    if(my_id == 0)
    {
        localDistances[0] = 0 ;   				//Selecting the source node
    }
 
 
    visited = new bool[localVertexCount];
 
    for(i = 0; i < localVertexCount; i++)			//Keeping the visited at each 
								//processor to be false initially
        visited[i] = false;
 
    minNode min;
    minNode globalMin ;
    visitedCount = 0;
    do								//staring the dijkstra algorithm
    {
 
        visitedCount++;
        min.value = INT_MAX;
        minValue = INT_MAX;
 
     
        for(i = 0; i < localVertexCount; i++)
        {
            if(localVisitedCount == localVertexCount)
            {   
                min.value = INT_MAX;			//If a process has finished processing 
							//all its vertices, we still need to run
							// this process without this the MPI 
							//function will not run. so we are 
							//assigning INT_MAX as local minimum
            						//for such processes. No further nodes 
							//will be selected from these processes.
		break;
	    }



 
            if((visited[i] == false) && (localDistances[i] < minValue) )
            {
                min.value = localDistances[i];		//Finding the local minimum
                min.index = localVertexCount*my_id + i; //Mapping the local minimum index to 
							//global index.
                minValue = localDistances[i];
            }
        }
 
        MPI_Allreduce(&min, &globalMin, 1,MPI_2INT, MPI_MINLOC,
			MPI_COMM_WORLD);			//Reduction to find 
								//global minimum. 
								//MPI_MINLOC is used to 
								//find global minimum
 								//and corresponding 
								//minimun index.
 
        int localIndex = globalMin.index - my_id*localVertexCount;	//Mapping global index 
									//to local index
 
        if((localIndex >= 0) && (localIndex < localVertexCount))	//checking the all 
									//reduced minimum belong
									// to which processor, 
									//setting that vertex 
									//to visited
        {  
            visited[localIndex] = true;
            localVisitedCount++;
        }
 
 
        for(i = 0 ; i < localVertexCount; i++)					//Relaxing edges
        {
            if((localAdjMat[i][globalMin.index] > 0) && (visited[i] == false))
            {
                if((globalMin.value + localAdjMat[i][globalMin.index]) < localDistances[i])
                    localDistances[i] = globalMin.value + localAdjMat[i][globalMin.index];
            }
        }
 
    }while(visitedCount != vertexCount);



    cout<<"Time Taken "<<MPI_Wtime()-start<<" seconds by procesoor number "<<my_id<<endl;

    int *overAllDistances;
    if(my_id == 0)
    {   
        overAllDistances = new int[vertexCount];			//Array to gather all the 
									//distance vector at one 
									//processor
        memset(overAllDistances, 0, sizeof(int)*vertexCount);

         for(int i = 0; i < numprocs; i++)
        {
            sendcount[i] = localVertexCount;   
            displ[i] = i*localVertexCount;
        }      
     
      							 
        sendcount[numprocs-1] = lastLocalVertexCount;		 //Giving different sized partition
								 //to the last processor  if size 
								 //is not multiple of numprocs
    }


    MPI_Gatherv(localDistances, localVertexCount, MPI_INT, overAllDistances, sendcount, displ, 
		 MPI_INT, 0, MPI_COMM_WORLD );   //Gathering keys from all processors for the final 
						//ouput
                                                                                                       
    if(my_id == 0)		
    {

        ofstream ofile;
        char filename[50];
        sprintf(filename,"Outputgraph%d_%d", vertexCount, density);
        ofile.open(filename);       
        ofile<<"Distance of nodes from 0th node "<<endl;
        for(int i = 0; i < vertexCount; i++)
        {
            ofile<<i<<"   "<<overAllDistances[i]<<endl; //printing output to a file
        }

        ofile.close();

        delete []overAllDistances;
        delete []sendcount;
        delete []displ;
    }
    delete []localDistances;
    delete []visited;  
    delete []*localAdjMat;
    delete []localAdjMat;   
    
    MPI_Finalize();
    return 0;
     
}
