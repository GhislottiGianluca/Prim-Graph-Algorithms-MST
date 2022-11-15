# mst.lisp Library
The mst.lisp file contains the programme that shows the minimum covering tree given an input graph, using Prim's algorithm.<br>

Since the algorithm uses a binary heap and functions for creating graphs, these can be used freely.<br>


## Legend 
* function --> returned value.
* / = from this character onwards is another output.
* '* (within the return) = substitutes a value that in the specifications of functions we do not have.
* ..... (within the return) = indicates a possible output continuum.



## Functions

### MST Management

* **mst-vertex-key** = executable only after the algorithm of prim on a graph *'g*, prints a key *k* (ex: 4) of a *'g* graph and an *'r* vertex.<br>
		    *NIL* if the graph does not exist, or if the prim algorithm has not previously been performed on the graph.<br>

	(mst-vertex-key 'g 'r) --> 4 / NIL

* **mst-previous** = executable only after the algorithm of prim on a graph *'g*, prints a parent vertex *'p* of a *g* graph and an *'r* vertex.<br>
		*NIL* if the graph does not exist, or if the prim algorithm has not previously been performed on the graph.<br>
		   
	(mst-previous 'g 'r) --> P / NIL


* **mst-prim** = executes the algorithm of prim a graph *'g* from a vertex *'source*.<br>
	       
	(mst-prim 'g 'source) --> NIL


* **mst-get** = prints a list of arcs, sorted according to 'preorder' and where two arcs have the same weight and are on the same level of the tree, there is a 'lexicographic' sorting of the vertices.<br>
	      *NIL* if Prim's algorithm has not been performed on the graph.<br>

	(mst-get 'g 'source)<br>
	--> ((ARC G SOURCE * *) (ARC G * * *) ......) / NIL

### Graph Management

* **is-graph** = checks whether the input *'g* graph is a graph. *NIL* if the graph does not exist.<br>
	       
	(is-graph 'g) --> G / NIL 


* **new-graph** = adds a new graph to the *graphs* hastable. <br>

	(new-graph 'g) --> G


* **delete-graph** = eliminates graph *'g*, with respective arcs and vertices.<br>

	(delete-graph 'g) --> NIL 


* **new-vertex** = adds a vertex *'f* to the graph *'g* and adds it to the hashtable.<br>
		 
	(new-vertex 'g 'f) --> (VERTEX G F)


* **graph-vertices** = returns a list of vertices of a graph *'g*. *NIL* if the graph does not exist or the list is empty.<br>
		     
	(graph-vertices 'g) --> ((VERTEX G *) (VERTEX G *) .... ) / NIL


* **new-arc** = inserts a new arc, possibly creating graphs and vertices that have not yet been created.<br>
		It does not insert loops, it updates the values of an arc if this is subsequently re-inserted.<br>
	      
	(new-arc 'g 'f 'd 4) --> (ARC G F D 4)


* **graph-arcs** = returns a list of arcs of a *'g* graph. *NIL* if the graph does not exist, or if the list is empty.<br>
		 
	(graph-arcs 'g) --> ((ARC G * * *) (ARC G * * *) .... ) / NIL 


* **graph-vertex-neighbours** = returns a list of adjacent arcs  of a vertex *'f*. *NIL* if the graph does not exist, or if the list is empty.<br>
			      
	(graph-vertex-neighbours 'g 'f)<br>
	--> ((ARCS G F * *) (ARCS G F * *) ... ) / NIL


* **graph-vertex-adjacent** = returns a list of vertices adjacent to a vertex *'f*. *NIL* if the graph does not exist or if the list is empty.<br>
			    
	(graph-vertex-adjacent 'g 'f)<br>
	--> ((VERTEX G *) (VERTEX G *) ..... ) / NIL


* **graph-print** = prints a list of vertices and arcs of a *'g* graph. *NIL* when the graph does not exist or if the list is empty.<br>
	
	(graph-print 'g)<br>
	--> ((VERTEX G *) (VERTEX G *) .... (ARC G * * *) ... ) / NIL


### Binary Heap Management

* **new-heap** = adds a new heap *'h* with capacity (optional) 4 in the *heaps* hashtable.<br>
	       
	(new-heap 'h 4) --> (HEAP H 4 #())


* **heap-delete** = deletes a heap *'h* from the *heaps* hashtable.<br>

	(heap-delete 'h) --> T 


* **heap-empty** = true if a heap *'h* does not contain any data. *NIL* otherwise.<br>
		 
	(heap-empty 'h) --> T / NIL


* **heap-not-empty** = says whether a heap *'h* contains data. *NIL* otherwise.<br>
		     
	(heap-not-empty 'h) --> T / NIL


*  **heap-head** = returns the *k* key and the value *v* of the first node of the heap. *NIL* if the heap is empty or does not exist.<br>
		
	(heap-head 'h) --> (k v) / NIL 

* **heap-insert** = inserts the node with key *k* and value *v* into the *'h* heap.<br>

	(heap-insert 'h k 'v) --> T


* **heap-extract** = extracts the head of a heap *'h*, deleting and returning the node, it also automatically restructures the heap in a correctly. *NIL* if the heap does not exist or is empty.<br>
		   
	(heap-extract 'h) --> (k v) / NIL 


* **heap-modify-key** = modifies the old key *'k* (ex: 5) with the new key *'u* (ex: 6) value *'v* in a heap *'h*. *NIL* if the heap or node or the old key does not exist.<br>
		      
	(heap-modify-key 'h 6 5 'v) --> T / NIL


*  **modify-key** = same function as the previous one (heap-modify-key), with the only difference being that the old key is not checked. Let us take as example numbers those of the previous one. *NIL* if the heap or node does not exist.<br>
	(modify-key 'h 6 'v) --> T / NIL


* **heap-print** = prints all nodes of a heap *'h*. *NIL* if the heap does not exist.<br>
		 
	(heap-print 'h)<br>
	--> #(#S(NODE :KEY * :VALORE *) (NODE :KEY * :VALORE *)) / NIL
