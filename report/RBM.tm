<TeXmacs|1.0.7.14>

<style|generic>

<\body>
  <with|font-series|bold|Restrictive Boltzmann Machines>

  In this chapter we will describe the theory behind Boltzmann Machines, as
  well as their advantages and how they can be used for classification.

  Hopfield networks are deterministc: training a network with the same
  patterns will yield the same weights, and matching a pattern against the
  network will always give the same result. This has the advantage of
  simplicity and ease of testing. However, as mentioned before, there are
  spirous attractors in the Hopfields network (liniear combination of an odd
  number of training patterns). Using stochastic updating rules will decrease
  the probability of being 'stuck' in such a state. This resembles simulated
  annealing, but ensuring we are not caught in a local minima (in the energy
  landscape).

  Restricted Boltzmann Machines have been used for various purposes in recent
  years (see http://uai.sis.pitt.edu/papers/11/p463-louradour.pdf,
  http://www.csri.utoronto.ca/~hinton/absps/nips00-ywt.pdf,
  http://www.csri.utoronto.ca/~hinton/absps/reluICML.pdf), most of it
  conducted by Geoffrey E. Hinton at university of Toronto, Canada. They have
  a simple structure: one layer of visible units and one layer of hidden
  units, which form a bipartite graph, as in Figure. The patterns used to
  train the network correspond to the visible (exposed units), while the
  hidden units correspond to the attributes of the features we would like to
  learn. It is worth mentioning that in the most common and simple form, both
  the visible and hidden units take binary values. This is the approach we
  also adopted for our implementation.

  \;

  Picture here

  \;

  \;

  The connections between the neurons in the two layers is symmetrical, so it
  can be represented as a weight matrix which keeps the connection beween
  hidden and visible layers. A state of a Restricted Boltzmann machins is
  given by the values of both the visibile and given state. The corresponding
  Hopfield energy formula for a state is given by :\ 

  <\equation*>
    E(v, h) = - <big|sum><rsub|i\<in\>V>a<rsub|i>*v<rsub|i
    ><rsub|>-<big|sum><rsub|j\<in\>H>b<rsub|j>*h<rsub|i
    >-<big|sum><rsub|i\<in\>V,\<nocomma\>j\<in\>H\<nocomma\>>*v<rsub|i
    >*h<rsub|j> w<rsub|<with|font-shape|italic|<with|font-shape|italic|ij>>>
  </equation*>

  \;

  where, <math|w <rsub|<with|font-shape|italic|<with|font-shape|italic|>>>>is
  the weight matrix, <math|a> and <math|b >are the vectors of biases
  corresponding to the visbile, respectibly hidden layers. As expected,
  <math|v >is vector of visible units and <with|font-shape|italic|h> is the
  vector of hidden units.

  We denote by <with|font-shape|italic|V> = {1, ..
  <with|font-shape|italic|length> <with|font-shape|italic|v>} , the indices
  of a visible vector and by <with|font-shape|italic|H> = {1, ..
  <with|font-shape|italic|length> <with|font-shape|italic|h>} the indices of
  a hidden vector.

  \;

  <with|font-series|bold|Learning in the Boltzmann Machine>

  The rules for the updating the Boltzmann machine are stochastic, as
  follows:

  <\equation*>
    p<around*|(|h<rsub|j>= 1<around*|\|| |\<nobracket\>>v|)>=
    \<sigma\><around*|(|b<rsub|j>+<big|sum><rsub|i\<in\>V>v<rsub|i>*w<rsub|ij>|)>
  </equation*>

  <\equation*>
    p<around*|(|v<rsub|i>= 1<around*|\|| |\<nobracket\>>h|)>=
    \<sigma\><around*|(|a<rsub|i>+<big|sum><rsub|j\<in\>H>h<rsub|j>*w<rsub|ij>|)>
  </equation*>

  \;

  where <math|\<sigma\> = <frac|1|1 \<noplus\>+e<rsup|x>><rsup|>>, is the
  logistic sigmoid function.

  \;

  \;
</body>

<\initial>
  <\collection>
    <associate|language|british>
  </collection>
</initial>