# [Review of graph theory and graph representations](https://chenmoucheng.github.io/43041/graph-theory-representations.html)

## Graph theory

The main reference here is Chapter 6 and 7 of the lecture notes on
『
[アルゴリズムとデータ構造](https://eduweb.sta.kanazawa-u.ac.jp//portal/Public/Syllabus/SyllabusSearchStart.aspx?lct_year=2019&fac_cd=52&lct_no=43002&je_cd=1)
』 by 松林昭先生, which we shall refer to as
*the lecture notes* in this handout. We briefly review some of the
most important definitions and facts that will be useful to us.

**Definition.** A
[*directed graph*](https://en.wikipedia.org/wiki/Directed_graph) is
[graph](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)) G
= (V,E) whose edges are
[ordered pairs](https://en.wikipedia.org/wiki/Ordered_pair), i.e.,
every edge e of G is of the form (s,t) for s,t ∈ V. Furthermore, a
[*directed acyclic graph (DAG)*](https://en.wikipedia.org/wiki/Directed_acyclic_graph)
is a directed graph without any
[directed cycles](https://en.wikipedia.org/wiki/Cycle_(graph_theory)#Directed_circuit,_cycle).

**Example** (from the lecture notes). Let G = (V,E) for V =
{a,b,c,d,e,f,g,h} and E = {(a,b), (a,c), (a,d), (b,d), (c,d), (d,e),
(c,f), (f,g), (f,h), (g,h)}. In the lecture notes, there is a
visualization of G as an undirected graph. To visualize G as a
directed graph, you can visit
[http://arborjs.org/halfviz](http://arborjs.org/halfviz/) and
copy-paste the following text onto the pane on the right.

```
a -> b
a -> c
a -> d
b -> d
c -> d
d -> e
c -> f
f -> g
f -> h
g -> h
```

The visualization produced by
[http://arborjs.org/halfviz](http://arborjs.org/halfviz/) is
interactive. For example, you can drag the vertices to make it look
like the (undirected) graph in the lecture notes. As you can see, the
edges in a directed graph are usually visualized using arrows, whereas
in an undirected graph, lines.

So far we have been discussing *simple graphs*, which are graphs whose
edges are sets.  We can generalize the notion to
[*multigraphs*](https://en.wikipedia.org/wiki/Multigraph), namely,
graphs whose edges are
[multisets](https://en.wikipedia.org/wiki/Multiset).  In contrast to
the lecture notes, we are mainly interested in
multigraphs. Unfortunately,
[http://arborjs.org/halfviz](http://arborjs.org/halfviz/) can only
visualize simple graphs. You can try other (graph) visualization
tools, such as [Cytoscape.js](https://js.cytoscape.org/), to see if
you can visualize multigraphs.

Graphs have numerous applications. In this experiment, we shall
explore a particular application of DAGs to
[*term unification*](https://en.wikipedia.org/wiki/Unification_(computer_science)#Syntactic_unification_of_first-order_terms).

**Definition.** Given a set V of *variable symbols*, a set C of
*constant symbols*, and sets Fn of *n-ary function symbols*, for each
positive integer n, the set of
[*(unsorted first-order) terms*](https://en.wikipedia.org/wiki/Term_(logic)#Formal_definition)
T is recursively defined to be the smallest set such that:

  - every variable symbol is a term: V ⊆ T;

  - every constant symbol is a term: C ⊆ T;

  - from every n terms t1,...,tn and every n-ary function symbol f ∈
  Fn, a term f(t1,...,tn) ∈ T can be built.

**Definition.** A *term DAG* is a directed acyclic multigraph whose
vertices are labeled with variable, constant, or function symbols,
whose outgoing edges from any vertice are *ordered*, and where the
outdegree of any vertice labeled with a function symbol f is equal to
the arity of f.

**Example.** Let V = {x0,x1,x2}, C = {c}, F1 = {f}, F2 = {g}, F3 =
{h}, F4 = F5 = ... = ∅. Then T={x0, x1, x2, c, f(x0), f(x1), f(x2),
f(c), g(x0,x0), g(x0,x1), ..., h(x0,x0,x0), h(x0,x0,x1), ...,
f(f(x0)), ..., g(f(x0),h(x1,x2,f(c))), ...}. You can download and run
the program
[`term-graph.exe`](https://media.githubusercontent.com/media/chenmoucheng/chenmoucheng.github.io/master/43041/code/term-graph.exe)
in [`cmd.exe`](https://ja.wikipedia.org/wiki/Cmd.exe); every time it
will output a random term (of appropriate size) from T, along with its
term DAG. Let us take a closer look at the following example output of
`term-graph.exe`.

```
; h(g(c,x2),f(x2),f(x0))
; 6
3 -> 2
4 -> 7
4 -> 2
5 -> 0
6 -> 4
6 -> 3
6 -> 5
```

First, let us try to recover the missing vertice labeling in the term
DAG, which is easy in such a toy example. For example, we observe that
vertice 6 should be labeled with the function symbol h, as it is the
only vertice with outdegree 3. Following the same line of reasoning,
vertice 4 should be labeled with g. Now, when it comes to the other
two outgoing paths 6→3→2 and 6→5→0, we know that both 3 and 5 should
be labeled with f, but it is unclear whether we should label 2 with x0
or x2 (and vice versa for vertice 0). We have a similarly ambiguous
situation when we try to label vertices 7 and 2 if we only look at the
outgoing edges 4→7 and 4→2 from their common source vertice. In this
example, fortunately, we have an additional piece of information that
the subgraphs { 4→7, 4→2 } and { 3→2 } share a common vertice 2, which
must correspond to the fact that the subterms `g(c,x2)` and `f(x2)`
also share a common subterm `x2`.  We can thus complete labeling all
the vertices as follows.

Vertice|0|2|3|4|5|6|7
:---|---:|---:|---:|---:|---:|---:|---:
Symbol|`x0`|`x2`|`f`|`g`|`f`|`h`|`c`

We can now visualize this 7-vertice term DAG, again using
[http://arborjs.org/halfviz](http://arborjs.org/halfviz/):

```
3"f" -> 2"x2"
4"g" -> 7"c"
4"g" -> 2"x2"
5"f" -> 0"x0"
6"h" -> 4"g"
6"h" -> 3"f"
6"h" -> 5"f"
```

We conclude this (rather lengthy) example by two important
observations.  First, we note that term DAGs are by no means unique;
sometimes a term can have two or more term DAGs. For example, you can
verify that the following term DAG *with 8 vertices* also corresponds
to the same term `h(g(c,x2),f(x2),f(x0))`:

```
3"f" -> 1"x2"
4"g" -> 7"c"
4"g" -> 2"x2"
5"f" -> 0"x0"
6"h" -> 4"g"
6"h" -> 3"f"
6"h" -> 5"f"
```

Last but not least, the ambiguity can be eliminated altogether if the
outgoing edges from any vertice are indeed *ordered*, as the
definition of term DAGs prescribes. However, this means that the
(outgoing) edges would be a *list*, rather than a multiset.

## Graph representations

Three representations of graphs are introduced in the lecture notes,
namely, *adjacency matrix*, *incidence matrix*, and *adjacency list*.

**Example.** An adjacency matrix for the last term DAG is:

```
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 1 0 0 0 0 0 0
0 0 1 0 0 0 0 1
1 0 0 0 0 0 0 0
0 0 0 1 1 1 0 0
0 0 0 0 0 0 0 0
```

If we forget about the directions of the edges, then an incidence
matrix for the same term DAG (when viewed as an undirected graph) is:

```
0 0 0 1 0 0 0
1 0 0 0 0 0 0
0 0 1 0 0 0 0
1 0 0 0 0 1 0
0 1 1 0 1 0 0
0 0 0 1 0 0 1
0 0 0 0 1 1 1
0 1 0 0 0 0 0
```

Since we are mainly interested in term DAGs, in which outgoing edges
are ordered, we shall focus on adjacency list representations, e.g.,
for the same term DAG:

```
(0,[])
(1,[])
(2,[])
(3,[1])
(4,[7,2])
(5,[0])
(6,[4,3,5])
(7,[])
```

**Definition.** A
  [*graph homomorphism*](https://en.wikipedia.org/wiki/Graph_homomorphism#Definitions)
  φ from a graph G = (V,E) to G' = (V',E') is a function from V to V'
  that *preserves* the graph structure of G; that is, for any edge e =
  (s,t) of G, (φ(s),φ(t)) is an edge of G'. Furthermore, a graph
  homomorphism is a
  [*graph isomorphism*](https://en.wikipedia.org/wiki/Graph_isomorphism)
  when its underlying vertice function is bijective.

Given a term, the `term-graph.exe` program can also generate a random,
isomorphic term DAG:

```
C:¥> term-graph
; h(g(c,x2),f(x2),f(x0))
; 6
3 -> 2
4 -> 7
4 -> 2
5 -> 0
6 -> 4
6 -> 3
6 -> 5
C:¥> term-graph 'h(g(c,x2),f(x2),f(x0))'
; 4
4 -> 7
4 -> 6
4 -> 5
5 -> 0
6 -> 2
7 -> 3
7 -> 2
```

It is easy to verify that these two graphs are isomorphic by the
following φ:

v|0|2|3|4|5|6|7
---:|---:|---:|---:|---:|---:|---:|---:
φ(v)|0|2|6|7|5|4|3

Now we apply φ to every vertice in the adjacency-list representation
of the term DAG in our running example and arrive at:

```
(0,[])
(2,[])
(3,[])
(4,[7,6,5])
(5,[0])
(6,[2])
(7,[3,2])
```

**Exercise.** The `term-graph.exe` program can also take two integers
  `v` and `e` as its parameters: `term-graph v e` will try to generate
  a random term whose term DAG roughly has `v` vertices and `e`
  edges. Run it a few times, generate a pair of larger, isomorphic
  term DAGs, and compute the isomorphism between them. Visualize both
  term DAGs using
  [http://arborjs.org/halfviz](http://arborjs.org/halfviz/) and
  convince yourself that isomorphic graphs are indeed "the same".