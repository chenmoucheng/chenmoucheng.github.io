#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <assert.h>

// The basic type of vertices

typedef int Vertex;

const char *readVertex(Vertex *v, const char *str)
{
    int off;
    sscanf(str, " %d %n", v, &off);
    return str + off;
}

void showVertex(Vertex v)
{
    printf("%d", v);
}

// Lists of vertices

typedef struct {
    unsigned int len;
    Vertex *sto;
} ListOfVertices;

int isEqListOfVertices(const ListOfVertices *vs, const ListOfVertices *us)
{
    if (vs->len != us->len) return 0;
    for (unsigned int i = 0; i < vs->len; i ++) {
        if (vs->sto[i] != us->sto[i]) return 0;
    }
    return 1;
}

void copyListOfVertices(ListOfVertices *vs, const ListOfVertices *us)
{
    vs->len = us->len;
    vs->sto = malloc(vs->len * sizeof(*vs->sto));
    for (unsigned int i = 0; i < vs->len; i ++) {
        vs->sto[i] = us->sto[i];
    }
}

void emptyListOfVertices(ListOfVertices *vs)
{
    vs->len = 0;
    vs->sto = malloc(sizeof(*vs->sto));
}

int isNullListOfVertices(const ListOfVertices *vs)
{
    return vs->len == 0;
}

void appendListOfVertices(ListOfVertices *vs, Vertex v)
{
    vs->len ++;
    vs->sto = realloc(vs->sto, vs->len * sizeof(*vs->sto));
    vs->sto[vs->len - 1] = v;
}

Vertex removeAtHeadListOfVertices(ListOfVertices *vs)
{
    Vertex v;

    assert(vs->len > 0);
    v = vs->sto[0];

    vs->len --;
    for (unsigned int i = 0; i < vs->len; i ++) {
        vs->sto[i] = vs->sto[i + 1];
    }
    vs->sto = realloc(vs->sto, vs->len * sizeof(*vs->sto));

    return v;
}

void filterListOfVertices(ListOfVertices *vs, int (*predicate)(Vertex), const ListOfVertices *us)
{
    emptyListOfVertices(vs);
    for (unsigned int i = 0; i < us->len; i ++) {
        Vertex v = us->sto[i];
        if (predicate(v)) appendListOfVertices(vs, v);
    }
}

const char *readListOfVertices(ListOfVertices *vs, const char *str)
{
    int off;

    sscanf(str, " %d %n", &vs->len, &off); str += off;
    vs->sto = malloc(vs->len * sizeof(*vs->sto));
    for (unsigned int i = 0; i < vs->len; i ++) {
        str = readVertex(&vs->sto[i], str);
    }

    return str;
}

void showListOfVertices(const ListOfVertices *vs)
{
    printf("%d", vs->len);
    for (unsigned int i = 0; i < vs->len; i ++) {
        printf(" ");
        showVertex(vs->sto[i]);
    }
}

void showListOfVertices2(const ListOfVertices *vs)
{
    printf("[");
    if (vs->len > 0) showVertex(vs->sto[0]);
    for (unsigned int i = 1; i < vs->len; i ++) {
        printf(",");
        showVertex(vs->sto[i]);
    }
    printf("]");
}

void freeListOfVertices(ListOfVertices *vs)
{
    free(vs->sto);
}

// Graphs in adjacency list representation

typedef struct {
    Vertex min,max;
    ListOfVertices *adj;
} Graph;

ListOfVertices *adj(const Graph *g, Vertex v)
{
    return g->adj + (v - g->min);
}

int isEqGraph(const Graph *g, const Graph *h)
{
    if (g->min != h->min || g->max != h->max) return 0;
    for (Vertex v = g->min; v <= g->max; v ++) {
        if (!isEqListOfVertices(adj(g, v), adj(h, v))) return 0;
    }
    return 1;
}

void copyGraph(Graph *g, const Graph *h)
{
    g->min = h->min;
    g->max = h->max;
    g->adj = malloc((g->max - g->min + 1) * sizeof(*g->adj));
    for (Vertex v = g->min; v <= g->max; v ++) {
        copyListOfVertices(adj(g, v), adj(h, v));
    }
}

const char *readGraph(Graph *g, const char *str)
{
    int off;

    sscanf(str, " %d \n%n", &g->min, &off); str += off;
    sscanf(str, " %d \n%n", &g->max, &off); str += off;
    g->adj = malloc((g->max - g->min + 1) * sizeof(*g->adj));
    for (Vertex v = g->min; v <= g->max; v ++) {
        str = readListOfVertices(adj(g, v), str);
        sscanf(str, " \n%n", &off);
        str += off;
    }

    return str;
}

void showGraph(const Graph *g)
{
    printf("%d\n", g->min);
    printf("%d\n", g->max);
    for (Vertex v = g->min; v <= g->max; v ++) {
        showListOfVertices(adj(g, v));
        printf("\n");
    }
}

void freeGraph(Graph *g)
{
    for (Vertex v = g->min; v <= g->max; v ++) {
        freeListOfVertices(adj(g, v));
    }
    free(g->adj);
}

// The basic type of edges

typedef struct {
    Vertex s;
    Vertex t;
} OrderedPairOfVertices;

typedef OrderedPairOfVertices Edge;

int isEqEdge(const Edge *e, const Edge *f)
{
    return e->s == f->s && e->t == f->t;
}

const char *readEdge(Edge *e, const char *str)
{
    int off;
    sscanf(str, " %d -> %d %n", &e->s, &e->t, &off);
    return str + off;
}

void showEdge(const Edge *e)
{
    printf("%d -> %d", e->s, e->t);
}

// Lists of edges

typedef struct {
    unsigned int len;
    Edge *sto;
} ListOfEdges;

int isEqListOfEdges(const ListOfEdges *es, const ListOfEdges *fs); // test if *es == *fs
void copyListOfEdges(ListOfEdges *es, const ListOfEdges *fs);      // copy fs to es
int isNullListOfEdges(const ListOfEdges *es);                      // test if *es == []
const char *readListOfEdges(ListOfEdges *es, const char *str);     // read es from str (optional)
void showListOfEdges(const ListOfEdges *es);                       // print es to stdout
void freeListOfEdges(ListOfEdges *es);                             // free the memory used by es

// The vertices of a graph
void vertices(ListOfVertices *vs, const Graph *g)
{
    unsigned int i;
    Vertex v;

    vs->len = g->max - g->min + 1;
    vs->sto = malloc(vs->len * sizeof(*vs->sto));
    i = 0; v = g->min;
    while (v <= g->max) {
        vs->sto[i ++] = v ++;
    }
}

// The edges of a graph
void edges(ListOfEdges *es, const Graph *g);

// The outdegree of a vertex of a graph
unsigned int outdegree(const Graph *g, Vertex v)
{
    return adj(g, v)->len;
}

// The indegree of a vertex of a graph
unsigned int indegree(const Graph *g, Vertex v);

// Builds a graph from vertex bounds and a list of edges (optional)
void buildG(Graph *g, const OrderedPairOfVertices *bounds, const ListOfEdges *es);

// Test if a list of vertices is a topological sort of a graph
int isaTopSort(const ListOfVertices *vs, const Graph *g);

// A topological sort of a graph
int topSort(ListOfVertices *vs, const Graph *g);

// Example uses

void die(const char *file, unsigned int line, const char *msg)
{
    printf("%s:%d: error: %s\n", file, line, msg);
    exit(-1);
}

int isEven(Vertex v)
{
    return v % 2 == 0;
}

int main(int argc, char *argv[])
{
    const char *input;
    Graph g, h;
    Edge e, f;
    ListOfVertices vs, us;

    if (argc != 2) {
        printf("Usage: %s graph_description_file\n", argv[0]);
        exit(-1);
    }

    input = mmap(0, 1000, PROT_READ, MAP_SHARED, open(argv[1], O_RDONLY), 0);
    if (input == MAP_FAILED) die(__FILE__, __LINE__, "cannot open file");

    readGraph(&g, input);
    showGraph(&g);

    copyGraph(&h, &g);
    assert(isEqGraph(&g, &h));

    freeGraph(&g);
    freeGraph(&h);

    readEdge(&e, "1 -> 2");
    showEdge(&e);
    printf("\n");
    f = e;
    assert(isEqEdge(&e, &f));

    readListOfVertices(&us, "6 1 2 3 4 5 6");
    showListOfVertices(&us); printf("\n");

    filterListOfVertices(&vs, isEven, &us);
    showListOfVertices(&vs); printf("\n"); removeAtHeadListOfVertices(&vs);
    showListOfVertices(&vs); printf("\n"); removeAtHeadListOfVertices(&vs);
    showListOfVertices(&vs); printf("\n"); removeAtHeadListOfVertices(&vs);
    showListOfVertices(&vs); printf("\n"); removeAtHeadListOfVertices(&vs); // assertion should fail

    return 0;
}
