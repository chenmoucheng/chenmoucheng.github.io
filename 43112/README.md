# 情報セキュリティＤ

## Solving the discrete logarithm problem (DLP) with Pollard's rho method

Let $G$ be a finite cyclic group of order n written multiplicatively.  Let $g$
be a generator of $G$.  In general, DLP is the problem of finding an integer $x$
such that $h=g^x$ given $h\in G$.  One way to solve DLP is to find a suitable
relation between $g$ and $h$, e.g., $g^ah^b=1_G$, in which case $x=−a/b\bmod n$.

One possible way to obtain such a relation is through collisions.  That is, if
we have $g^{a_1}h^{b_1}=g^{a_2}h^{b_2}$ for $a_1\neq a_2$ and $b_1\neq b_2$,
then we can immediately obtain $g^{a_1−a_2}h^{b_1−b_2}=1_G$.  Pollard's rho
method is a way to create collisions.  The idea is to consider a function $f$
from $G$ to itself and its iterated functions:

$$ f^{\circ m}:=\underbrace{f\circ f\circ\cdots\circ f}_{m\text{
times}},\text{or }f^{\circ m}(x)=\underbrace{f(f(\cdots f(}_{m\text{
times}}x)\cdots )). $$

Starting from an element $x$ ∈ G, let us consider the *orbit* of $x$ under the
iteration of $f$:

$$ \left\{x_i:=f^{\circ
i}(x)\,\middle|\,i=0,1,2,\ldots\right\}=\left\{x,f(x),f(f(x)),\ldots\right\}. $$

What would this orbit look like for random $f$ and random $x$?  Since $G$ is
finite, it cannot contain any infinite non-repeating orbits.  Thus any orbit
will necessarily run into repetition at some point, leading to a picture like
this:

<img src="https://upload.wikimedia.org/wikipedia/commons/4/47/Pollard_rho_cycle.jpg" alt="Pollard rho cycle.jpg" height="480" width="520">

This looks like the Greek letter ρ and hence the name "rho method."  On average,
both the tail and the circle parts of ρ are of length $\mathcal O(\sqrt n)$ due
to the usual birthday argument, so the complexity of the algorithm is $\mathcal O(\sqrt n)$. 

How do we recover the discrete logarithm from a collision $x_i=x_j$?  Well, if
we remember how each $x_i$ is represented as $g^{a_i}h^{b_i}$, then we can
simply rewrite the collision as

$$ g^{a_i}h^{b_i}=g^{a_j}h^{b_j} $$

and compute the discrete logarithm as mentioned above.

What kind of function $f$ can be used?  We need some function whose behavior is
close to being "random."  In his seminal work, Pollard suggested to use some
simple function like this.  First let us partition $G$ as the disjoint union of
three subsets $S_1$, $S_2$, and $S_3$ such that it is easy to figure out to
which subset any given element belongs.  Then we let

$$ f(x) = \left\{\begin{aligned}
hx\text{ if }x\in S_1, \\
x^2\text{ if }x\in S_2, \\
gx\text{ if }x\in S_3. \\
\end{aligned}\right. $$

Here we need to be careful, for example, not to let $S_2$ contain $1_G$;
otherwise we would be "trapped" in $S_2$ after visiting $1_G$.  Also, from the
decomposition of $x=g^ah^b$, we can easily compute that of $f(x)=g^{a'}h^{b'}$
and hence keep track of this decomposition throughout all iterations.  In
general, we can use a different number of partitions or a different iteration
function, as long as the function behaves close to being "random" and contains
no "traps."  This is particularly important when we parallelize Pollard's rho
method, as we would like each parallel worker to compute something "useful" as
opposed to repeat one another's computation, in which case we could not learn
any new information.

Last but not least, we also need a mechanism to detect when there is a
collision.  A naive way is to store all the elements we have computed in a
table; if we keep the table sorted, then for each new element, we just need
$\mathcal O(\log\sqrt n)$ time to insert the element and see whether there is a
collision or not.  However, this requires $\mathcal O(\sqrt n)$ space for
storing the elements.  On the other end of the spectrum, we have Floyd's
cycle-finding algorithm, which requires very little space at the cost of a
little bit extra computation: $\mathcal O(c\sqrt n)$ for some small constant
$c$.  The idea is simple: imagine that we have a tortoise that computes $x_i$
for $i=0,1,2,\ldots$, as well as a hare that computes twice as fast $x_{2i}$ for
$i=0,1,2,\ldots$  What would happen when the tortoise and the hare race against
each other on a ρ-like orbit as shown above?  No matter where they start,
eventually both of them will get into the cycle part, and then before the
tortoise can finish his first lap, the fast hare will inevitably catch up and
collide with it.  This way we just need to remember two elements $(x_i,x_{2i})$
at the cost of computing $f$ for three times at each step.  Furthermore, while
the naive collision-detection mechanism can be easily parallelized, Floyd's
cycle-finding algorithm has a sequential nature.

We have seen that the naive cycle-detection and Floyd's cycle-finding algorithm
are on the two ends of the spectrum in terms of space-time trade-off.  For
achieving somewhere in between, we can use the idea of *distinguished elements*
due to Rivest.  The set of distinguished elements is a subset whose membership
is very easy to test.  Then instead of storing every element we have computed,
we can simply store the distinguished elements in a sorted table.  Just like the
naive cycle-detection algorithm, this is also highly parallelizable.
Furthermore, if the size of the set of distinguished elements is $n/d$ for some
$d\ll\sqrt n$, then the space complexity is $\mathcal O(\sqrt n/d)$, whereas the
time complexity is $\mathcal O(\sqrt n+dp)$ for $p$ parallel workers (ignoring
logarithmic factors).

---

This homework assignment is about solving small instances of DLP.  After filling
in your name and generating challenges, you will try to solve as many of them as
possible by implementing Pollard's rho method using any programming language(s)
of your choice.  Note that if you choose to use the C programming language, then
you may need to use an arbitrary precision arithmetic library such as
[libgmp](https://gmplib.org/), which has some nice tutorials such as
[this](https://home.cs.colorado.edu/~srirams/courses/csci2824-spr14/gmpTutorial.html).
To help debugging, you can calculate relevant modular exponentiations by
clicking the "Hint:" buttons below.

You need to turn in a detailed report documenting how and why you are able (or,
in the case of failure, unable) to solve these DLP instances on LMS before
February 16, 2021.

Name: <textarea id="name" rows="1" cols="24">Chen-Mou Cheng</textarea> <button
type="button" onclick="generate_challenges()">Generate DLP challenges</button>

1. (40%) Let $G$ be the multiplicative group $(\mathbb Z/101\mathbb Z)^\times$.
   Let $g=2$ and $h=3$.  Let $f:G\rightarrow G$ be such that

   $$ f(x) = \left\{\begin{aligned}
   hx\text{ if }x\in S_1:=\{0\leq x<101\,|\,x=1\bmod 101\}, \\
   x^2\text{ if }x\in S_2:=\{0\leq x<101\,|\,x=2\bmod 101\}, \\
   gx\text{ if }x\in S_3:=\{0\leq x<101\,|\,x=3\bmod 101\}. \\
   \end{aligned}\right. $$

   Find an $x$ such that $2^x=3\bmod 101$ using Pollard's rho method by
   completing the following table.

   $$ \begin{array}{lll|l}
   i & a_i & b_i & 2^{a_i}3^{b_i}\bmod 101 \\ \hline\hline
   0 & 0 & 0 & 1 \\
   1 & 0 & 1 & 3 \\
   2 & 1 & 1 & 6 \\
   3 & 2 & 1 & 12 \\
   4 & 3 & 1 & 24 \\
   5 & 4 & 1 & 48 \\
   6 & 5 & 1 & 96 \\
   7 & 6 & 1 & 91 \\
   & \vdots & & \vdots
   \end{array} $$

2. (40%) <span id="h1">?</span> = 13008203 <sup>?</sup> (mod 28524863)
   <p><button type="button"
   onclick="modular_exponentiate('base1','exp1','mod1','res1')">Hint:</button>
   <span id="base1">13008203</span> ^ <textarea id="exp1" rows="1"
   cols="9">14262431</textarea> = <span id="res1">1</span> (mod <span
   id="mod1">28524863</span>)
3. (20%) <span id="h2">?</span> = 11391220849310 <sup>?</sup> (mod
   1070407397926837)
   <p><button type="button"
   onclick="modular_exponentiate('base2','exp2','mod2','res2')">Hint:</button>
   <span id="base2">11391220849310</span> ^ <textarea id="exp2" rows="1"
   cols="17">29733538831301</textarea> = <span id="res2">1</span> (mod <span
   id="mod2">1070407397926837</span>)
4. (Bonus) <span id="h3">?</span> = 657139733149567003766 <sup>?</sup> (mod
   1077984309859658267861)
   <p><button type="button"
   onclick="modular_exponentiate('base3','exp3','mod3','res3')">Hint:</button>
   <span id="base3">657139733149567003766</span> ^ <textarea id="exp3" rows="1"
   cols="22">1738684370741384303</textarea> = <span id="res3">1</span> (mod <span
   id="mod3">1077984309859658267861</span>)

<script type="text/javascript" charset="utf-8" src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML, https://vincenttam.github.io/javascripts/MathJaxLocal.js"></script>
<script type="text/javascript" src="./biginteger.js"></script>
<script type="text/javascript" src="./script.js"></script>
