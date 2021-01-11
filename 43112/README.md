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

<script type="text/javascript" charset="utf-8" 
src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML,
https://vincenttam.github.io/javascripts/MathJaxLocal.js"></script>
