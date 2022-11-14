# mst.pl Library 

The file mst.pl contains the programme that shows the minimum covering tree of a graph, using Prim's algorithm.

For its realisation, we used a binary heap and functions for the creation of graphs.


## Legend
* function(). --> return. 
* \n = from this character onwards will be printed on a new line.
* / = from this character onwards is another output.
* * (nei return) = indicates a piece of data that makes up the output but which we do not have in the specification.
* ..... (all'interno del return) = indicates a possible output continuum.


## Functions

### MST management

* **list_mst** = prints out all *vertex_keys* and *vertex_previous* of a *g* graph, after the execution of the **prim algorithm**.<br>
	       
	list_mst(g). --> vertex_key(g, *, *) \n vertex_previous(g, *, *)


* **mst_prim** = executes **Prim's algorithm** on a graph *g* starting from a *vertex source*.<br>

	mst_prim(g, source). --> true 


* **mst_get** = prints a list of arcs, sorted according to 'preorder' and where two arcs have the same weight and are on the same level of the tree, there is a 'lexicographic' sorting of the vertices. <br>
	
	mst_get(g, source, Xs) --> Xs = [arc(g, *, *, *), .....] 


### Graph Management 
* **new_graph** = asserts a *g* graph in the database.

	new_graph(g). --> true


* **delete_graph** = eliminates a graph *g* and its respective arcs and vertices.<br>

	delete_graph(g). --> true


* **new_vertex** = asserts a vertex *v* in a graph *g* in the database false if the graph *g* does not exist.<br>
		 
	new_vertex(g, v). --> true / false


* **graph_vertices** = prints a list of all vertices of a *g* graph false if no *g* graph exists.<br>
		     
	graph_vertices(g, Xs).<br>
	--> Xs = [vertex(g, *), vertex(g, *), .....] / false


*  **list_vertices** = prints all vertices of a *g* graph false if no *g* graph exists.<br>
		    
	list_vertices(g).<br>
	--> vertex(g, *). \n vertex(g, *). \n ......... / false 


* **new_arc** = asserts a new arc of a graph *g* and two vertices *r* and *t*, with a weight (ex: 5).<br>
		False if there is no graph *g* or one of the two vertices *r* and *t*.<br>
	      
	new_arc(g, r, t, 5). --> true / false


* **graph_arcs** = prints a list of arcs of a g graph, false if no *g* graph exists.<br>
		 
	graph_arcs(g, Xs).<br>
	--> Xs = [arc(g, *, *, *), arc(g, *, *, *), .....] / false 


* **vertex_neighbours** = prints a list of vertices adjacent to a vertex *t* of a graph *g*, false if there is no graph *g* or vertex *t*.<br>
			
	vertex_neighbours(g, t, Xs). --> [arc(g, t, *, *), ......] / false


* **adjs** = returns a list of vertices adjacent to a vertex *t* of a graph *g*, false if there is no such graph *g* or vertex *t*.<br>
	   
	adjs(g, t, Xs). --> Xs = [vertex(g, *), ....] / false


* **list_vertices** = prints all the arcs of a *g* graph false if no *g* graph exists.<br>
		    
	list_arcs(g).
	--> arcs(g, *). \n arcs(g, *, *, *). \n ......... / false 


* **list_graph** = prints all vertices and all arcs of a graph *g*, *false* if no *g* graph exists.<br>
		 
	list_graph(g). --> vertex(g, *) \n ...... \n arc(g, *, *, *) \n ...... 


* **read_graph** = imports the contents of a 'filename.csv' file into a *g* graph.<br>
		   Its call overwrites the new graph with the old one, obviously in the case where the graph names coincide.<br>
		   *Note* = if you are not, in the user agent, in the folder where the file 'filename.csv' is located, you must type 'path/filename.csv'.<br>

	read_graph(g, 'filename.csv'). --> true


* **write_graph** = writes to a file 'filename.csv', all the arcs of a *g* graph.<br>
		    *False* if the file does not exist or if the path is incorrect.<br>
		    *Note* = if you are not, in the user agent, in the folder where the file 'filename.csv' is located, you must type 'path/filename.csv'.
		
	write_graph(g, 'filename.csv', graph). --> true /false<br>
	write_graph(g, 'filename.csv', edge) --> true / false<br>
	write_graph(g, 'filename.csv'). --> true /false


### Heap management

* **new_heap** = asserts a heap *h* in the database.<br>

	new_heap(h). --> true


* **delete_heap** = deletes the entire contents of a heap *h* from the database.<br>

    	 delete_heap(h). --> true 


* **heap_has_size** = prints the value of a heap *h* (ex: 5) false if no heap *h* exists.<br>
		    
	heap_has_size(h, S). --> S = 5 / false


* **heap_empty** = *true* if heap *h* is empty, *false* otherwise.<br>

	heap_empty(h). --> true / false


* **heap_not_empty** = *true* if heap *h* is not empty, *false* otherwise.<br>

	heap_not_empty(h). --> true / false


* **heap_head** = returns the first *key-value* node in a heap *h*, *false* if no heap *h* exists or if it is empty.<br>
		
	heap_head(h, K, V). --> K = 4, \n V = r / false


* **list_heap** = prints a heap *h* and all its nodes false if the heap does not exist.<br>
		
	list_heap(h).<br>
	--> heap(h, *). \n heap_entry(h, *, *, *) \n ..... / false


* **modify_key** = modifies the key of a node (ex: 3,5,r), replacing it.<br>
		   It then reorders the heap correctly.<br>
		 
	modify_key(h, 3, 5, r). --> true 


* **heap_insert** = inserts a node (ex: (5,r)) into a heap *h*, *false* if the heap *h* does not exist.<br>
		  
	heap_insert(h, 5, r). --> true / false


* **heap_extract** = extracts the first element of a heap *h*.<br>
		     By placing in *K* the weight (ex: 5) and in *V* the value (ex: r) of the node. <br>

	heap_extract(h, K , V). --> K = 5 \n V = r.
