#include <iostream>
#include <vector>
#include <set>
#include <cmath>
#include <cstdlib>
#include <climits>
#include <fstream>
#include <ctime>
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
            v = i;            //If and edge exists from v to i  
            if(!visited[v])
                dfs(adjMat,visited,v, vertices);
        }
    }
    //return count;
}

/* Using dfs after creation of graph to check if it is connected or not
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

/* Using dfs after creation of graph to check if it is connected or not
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
    // checking for configuration for which connected graph is not possible
    
    if (final_edge_count < (vertices-1))
    {  
        cout<<"connected graph not possible for this value of vertices and density" <<endl;
        return false;
    }
    set<int>  :: iterator it1;
    vector<set<int> > edges;
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
 
 	//Adding edge to the Adjacency matrix if the edge has not already been added.
 
            if((already_inserted == false) && (vertex1 != vertex2))
            {
                edges[vertex1].insert(vertex2);
                adjMat[vertex2][vertex1] = weight;
                edge_count++;
            }
        }while(final_edge_count != edge_count);//checking if the graph is connected or not
    }while(!check_connected(adjMat,vertices));
 
 
  //Writing the created Adjacency matrix to a file for future use
    
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
 
int readFile(int **adjMat, char* filename)
 
{
    fstream infile;
    infile.open(filename);
    cout<<infile.good();
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
    int * array = new int[rows*columns];  // Allocating continous memory
    int **data = new int*[rows];
    for(int i = 0; i < rows; i++)
        data[i] = &(array[i*columns]);    // putting two ints in each row of the 2d array.
    return data;
 
}
 
int main(int argc, char * argv[])
{  
 


    int my_id, numprocs;
    //char* filename = argv[3];
    int vertexCount = atoi(argv[1]);
    int density =  atoi(argv[2]); 
    int **adjMat = NULL;
    int *localDistances = NULL;
    bool * visited = NULL;
    int i;
    int visitedCount;
    int minValue;
 
     
    adjMat = allocate2DArray(vertexCount,vertexCount);
    for(int i = 0; i < vertexCount; i++)
            memset(adjMat[i], 0 , sizeof(int)*vertexCount);
    createRandomGraph(adjMat,vertexCount,density);
    //    readFile(adjMat, filename);
       
 
  
     clock_t    start;
     start = clock();  
    localDistances = new int[vertexCount];
 
    for(int i = 0 ; i < vertexCount; i++)
        localDistances[i] = INT_MAX;
 
   
        localDistances[0] = 0 ;   //Selecting the source node
    
 
 
    visited = new bool[vertexCount];
 
    for(i = 0; i < vertexCount; i++)
        visited[i] = false;
 
    minNode min;
    minNode globalMin ;
    visitedCount = 0;
    do
    {
 
        visitedCount++;
        min.value = INT_MAX;
        minValue = INT_MAX;
 
     
        for(i = 0; i < vertexCount; i++)
        {
      
            if((visited[i] == false) && (localDistances[i] < minValue) )
            {
                globalMin.value = localDistances[i];	//Finding the local minimum
                globalMin.index = i;			//Mapping the local minimum index to global
                minValue = localDistances[i];
            }
        }
 
         visited[globalMin.index] = true;
 
        for(i = 0 ; i < vertexCount; i++)
        {
            if((adjMat[i][globalMin.index] > 0) && (visited[i] == false))
            {
		//Relaxing edges
                if((globalMin.value + adjMat[i][globalMin.index]) < localDistances[i])
                    localDistances[i] = globalMin.value + adjMat[i][globalMin.index];
 
            }
        }
 
    }while(visitedCount != vertexCount);



     std::cout<<"Time taken "<<(clock() - start)/(double)(CLOCKS_PER_SEC)<<" seconds" <<endl;

        ofstream ofile;
        char filename[50];
        sprintf(filename,"Outputgraph%d_%d", vertexCount, density);
        ofile.open(filename);       
        ofile<<"Distance of nodes from 0th node "<<endl;
        for(int i = 0; i < vertexCount; i++)
        {
            ofile<<i<<"   "<<localDistances[i]<<endl;
        }

        ofile.close();

    delete []localDistances;
    delete []visited;  

    return 0;
     
}
