# Basic data structures for handling graphs

## Review of basic C programming

To represent, e.g., graphs with a custom datatype in C, we typically
use `typedef` to introduce our new datatype, along with a customary
set of functional and managerial routines for manipulating instances
of the new datatype. For example, we may need to test whether two
instances of the new datatype are *equal*: the C programming language
only supports equality on its primitive datatypes, so for more
complicated, derived datatypes such as `struct`, we would need to
implement our own equality tests. For datatypes whose storage size
cannot be determined at compile time, we would also need dynamic
memory management for creating, copying, as well as destroying the
instances. Last but not least, we may also want to provide
input/output functionalities for our new datatypes so that we can work
and debug with them more easily.

**Example.**
```
typedef ... MyType;

int isEqMyType(const MyType *x, const MyType *y);   // x == y?
void copyMyType(MyType *x, const MyType *y);        // x <- y
const char *readMyType(MyType *x, const char* str); // x <- str
void showMyType(const MyType *x);                   // IO <- x
void freeMyType(MyType *x);                         // delete x

// example uses

MyType x, y;

readMyType(&x, input_string);
showMyType(&x); // should look the same as input_string
.
.
.
copyMyType(&y, &x);
assert(isEqMyType(&x, &y));
.
.
.
freeMyType(&x); 
freeMyType(&y); 

```

## A more detailed example

Let us take a closer look at the following
[example code](./code/sample.c). In this example, we have defined
custom datatypes for vertices and edges, as well as lists of vertices
and edges. Per our discussion above, we need dynamic memory management
for lists but not for vertices and edges, as their storage sizes are
known at compile time. With these datatypes, we can now define our
`Graph` datatype using the adjacency list representation:

```
typedef struct {
    Vertex min,max;
    ListOfVertices *adj;
} Graph;

ListOfVertices *adj(const Graph *g, Vertex v)
{
    return g->adj + (v - g->min);
}
```

Here we assume that our vertices are numbered contiguously from `min`
to `max`, and `adj(&g, v)` gives us the list of adjacent vertices of a
vertex `v` connected by its outgoing edges in a graph `g`. We note
that the two arguments are passed to `adj` differently: `Graph g` is
passed by *reference*, whereas `Vertex v`, by *value*. In C
programming, it is customary to pass instances of primitive datatypes
by value and instances of more complicated, derived datatypes by
reference, as is the case in this example.

**Homework.** Implement the following five functions for handling
  lists of edges and give a few convincing test cases to show the
  correctness of your implementation.

```
int isEqListOfEdges(const ListOfEdges *es, const ListOfEdges *fs); // test if *es == *fs
void copyListOfEdges(ListOfEdges *es, const ListOfEdges *fs);      // copy fs to es
int isNullListOfEdges(const ListOfEdges *es);                      // test if *es == []
void showListOfEdges(const ListOfEdges *es);                       // print es to stdout
void freeListOfEdges(ListOfEdges *es);                             // free the memory used by es
```

## Topological sorts of a directed graph, revisited

 We recall that a topological sort of a directed graph G = (V,E) is a
linear ordering on V such that for every edge e ∈ E with s(e) = u and
t(e) = v, u ≤ v in the ordering. Here we give a common way for
computing a topological sort of a directed graph. For that, we will
need the following definition.

**Definition.** The *indegree* of a vertex v ∈ V in a directed
  multigraph G = (V,E) is the number of edges going into v, i.e., |{ e
  ∈ E : t(e) = v }|. Similarly, the *outdegree* of v is the number of
  edges going out from v, i.e., |{ e ∈ E : s(e) = v }|.

**Algorithm ([Kahn, 1962](https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm)).**

```
Input: Graph G = (V,E)
emptyListOfVertices(&L);
ListOfVertices S ← [ Vertex v | v ← vertices(&G) && indegree(&G, v) == 0 ];
while (!isNullListOfVertices(&S)) {
    Vertex u ← removeAtHeadListOfVertices(&S);
    appendListOfVertices(&L, u);
    foreach [ Vertex v | (u,v) ∈ E ] {
	    G ← (V, E \ {(u,v)});
	    if (indegree(&G, v) == 0) appendListOfVertices(&S, v);
	}
	G ← (V \ {u}, E);
}
return isNullListOfEdges(&E);
Output: ListOfVertices L (when E = ∅)
```

To prove the correctness of Kahn's algorithm, we need to show that it
terminates in a finite number of steps (for a finite input `Graph G`),
and the output `L` represents a topological sort of the input `Graph
G` when `G` is a DAG and when the algorithm terminates. First, we note
that at each step of the while loop, the number of vertices of `G`
monotonically decreases, so the algorithm must terminate after a
finite number of steps. Next, for every edge `(u,v) ∈ E`, Kahn's
algorithm would never insert `v` before `u` into `L` because a vertex
`v` needs to be in `S` before it can be inserted into `L`, which
happens if and only if its indegree is or becomes 0, at which point
all vertices `u` preceding `v` would have already been inserted into
`L`. Finally, the algorithm terminates when there are no vertices of
indegree 0 in `G`. At this time, `G` is either empty or not a DAG
according to the following two lemmas, whose proofs we leave as an
exercise.

**Lemma.** A (finite) DAG has at least one vertex of indegree 0.

**Lemma.** If we remove one or more vertices/edges from a DAG, then we
  would have another, possibly empty, DAG.

**Homework.** Implement the following four functions and give a few
  convincing test cases to show the correctness of your
  implementation. You can use the
  [web version](https://term-graph-iedy2lhg3a-an.a.run.app/) of the
  `term-graph.exe` program to check whether a list of vertices is a
  topological sort of a graph or not.


```
// The edges of a graph
void edges(ListOfEdges *es, const Graph *g);

// The indegree of a vertex of a graph
unsigned int indegree(const Graph *g, Vertex v);

// Test if a list of vertices is a topological sort of a graph
int isaTopSort(const ListOfVertices *vs, const Graph *g);

// A topological sort of a graph
int topSort(ListOfVertices *vs, const Graph *g);

```

