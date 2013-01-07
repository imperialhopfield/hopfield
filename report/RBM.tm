<TeXmacs|1.0.7.14>

<style|article>

<\body>
  <with|font-series|bold|Restricted Boltzmann Machines>

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
  years (TODO add to bib see http://uai.sis.pitt.edu/papers/11/p463-louradour.pdf,
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

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ <big-figure|<image|rbm1.png|300px|300px||>|Restricted
  Boltzmann Machine>

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
    >*h<rsub|j> w<rsub|<with|font-shape|italic|<with|font-shape|italic|i j>>>
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

  \ <with|font-series|bold|Probability of a state>

  After traning, the network assigns a probability to each possible state, as
  follows:

  <\equation*>
    p<around*|(|v,h|)> = <frac|1|Z>e<rsup|-E<around*|(|v,h|)>>
  </equation*>

  where Z is used to normalize the probabilities\ 

  <\equation*>
    <math|Z = <big|sum><rsub|v,h><rsub|><rsub|><rsub|><rsub|><rsub|><rsub|>e<rsup|-E<around*|(|v,h|)>>>
  </equation*>

  Thus, the porbability the network assings for a visibile vector
  <with|font-shape|italic|v>:

  <\equation>
    p<around*|(|v|)> = <big|sum><rsub|h><frac|1|Z>e<rsup|-E<around*|(|v,h|)>>
  </equation>

  <with|font-series|bold|Training the network>

  The network can be trained such that one maximizes the probabilty of a
  traning pattern. The derivative of the log probability of a trainig vector
  (given by (1)), is simple, as follows:

  <\equation*>
    <frac|\<delta\>log p<around*|(|v|)>|\<delta\>w<rsub|ij>>=
    <around*|\<langle\>|v<rsub|i>*h<rsub|i>|\<rangle\>>
    <rsub|data>-<around*|\<langle\>|v<rsub|i>*h<rsub|i>|\<rangle\>><rsub|model>
  </equation*>

  Due to the bipartitate nature of the Restricted Boltzmann Machine, it is
  easy to attain an <with|font-shape|italic|unabiased> sample of the hidden
  units, given the visible units.

  <\equation>
    p<around*|(|h<rsub|j>= 1<around*|\|| |\<nobracket\>>v|)>=
    \<sigma\><around*|(|b<rsub|j>+<big|sum><rsub|i\<in\>V>v<rsub|i>*w<rsub|i
    j>|)>
  </equation>

  For the visible units, the same formula gives a
  <with|font-shape|italic|biased> sample of the visible units.

  <\equation>
    p<around*|(|v<rsub|i>= 1<around*|\|| |\<nobracket\>>h|)>=
    \<sigma\><around*|(|a<rsub|i>+<big|sum><rsub|j\<in\>H>h<rsub|j>*w<rsub|i
    j>|)>
  </equation>

  \;

  where <math|\<sigma\> = <frac|1|1 \<noplus\>+e<rsup|-x>><rsup|>>, is the
  logistic sigmoid function.

  There are ways of getting an unbiased sample of the visible units, but they
  are very time consuming. In practice, the contrastive divergence algorithm
  is used as a faster substitute. The visibile units are set to a training
  vector and the binary states of the hidden vector are computed using (2).
  These binary units are used to <with|font-shape|italic|reconstruct> the
  states of the visible vector using (3).

  The training rule then becomes:

  <\equation*>
    \<Delta\>w<rsub|ij>= \<lambda\><around*|(|<around*|\<langle\>|v<rsub|i>*h<rsub|i>|\<rangle\>><rsub|data>-<around*|\<langle\>|v<rsub|i>*h<rsub|i>|\<rangle\>><rsub|reconstruction>|)>
  </equation*>

  We note that the above training rule has the desired properties for
  modelling the biological brain: it is both <with|font-shape|italic|local>
  and <with|font-shape|italic|incremental>. Angle brackets \ denote the
  expected value under the given distribution by the following subscript.

  \;

  <with|font-series|bold|Using Restricted Boltzmann Machines for
  classification>

  As suggested by Hinton in section 16 http://www.cs.toronto.edu/~hinton/absps/guideTR.pdf,
  there are various ways of using Restricted Boltzmann Machines for
  classification. We have employed two methods, one which we developed
  ourselves and one third one descriebed in the paper, by adding another set
  of visible units, the classification units.\ 

  \;

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ <big-figure|
  \ \ \ \ \ \ \ \ \ \ <image|diagram2mid.png|350px|350px||>|Classification
  Boltzmann machine>

  \;

  The vector hidden units is used to model the joint distribution between a
  vector of inputs v and a target (classification) vector y. The
  classification vector y corresponds to a class. It's length is given by the
  number of classes. <math|y = e<rsub|c>>, where c is the class the vector is
  modelling (<math|e<rsub|c>> is a vector with all zeros, expect from the
  position c, where it is 1).

  As seen from Figure 2, one now needs to weight matrices, which we will
  denote by <with|font-shape|italic|W>(the weight matrix between the visbile
  units and hidden ones) \ and U (the weight matrix and between the
  classification units and hidden ones). Vectors <with|font-shape|italic|b>,
  <with|font-shape|italic|c>, <with|font-shape|italic|d> correspond to the
  vector of biases for the visible units (training patterns), hidden units
  and classification units, respectively.

  The energy function for this model is:

  <\equation*>
    E<around*|(|v\<nocomma\>,y,h|)>= -d<rsup|T>*y-c<rsup|T>*h-b<rsup|T>v-h<rsup|T>*W
    v- h<rsup|T>*U y\ 
  </equation*>

  Which gives rise to the following formula for the probility of a
  configuration <with|font-shape|italic|v>, <with|font-shape|italic|y>.

  <\equation*>
    p<around*|(|v,y,h|)>=<frac| e<rsup|-E<around*|(|v\<nocomma\>,y,h|)>>|Z>
  </equation*>

  where <with|font-shape|italic|Z> is the normalizing constant. Thus,

  <\equation*>
    p<around*|(|v,y|)>=<big|sum><rsub|h>p<around*|(|v,y,h|)> =
    <big|sum><rsub|h><frac| e<rsup|-E<around*|(|v\<nocomma\>,y,h|)>>|Z>
  </equation*>

  The distribution used for the Classification Boltzmann Machine are the
  following:

  <\equation>
    p<around*|(|h<rsub|i>=1 <around*|\||v,y|\<nobracket\>>|)>=
    \<sigma\><around*|(|c<rsub|j>+ \ \<noplus\>\<noplus\>W<rsub|j>*v\<noplus\>+U<rsub|j>*y
    |)>
  </equation>

  <\equation>
    p<around*|(|v<rsub|i>=1 <around*|\||h|\<nobracket\>>|)>=
    \<sigma\><around*|(|b<rsub|j>+ \ \<noplus\>\<noplus\>h<rsup|T>*W<rsup|i>*
    |)>
  </equation>

  <\equation>
    p<around*|(|y = e<rsub|c><around*|\||h |\<nobracket\>>|)>=
    <frac|e<rsup|d<rsup|T>*e<rsub|c>\<noplus\>+h<rsup|T>*U*e<rsub|c>>|N>
  </equation>

  where N is the normalizing constant <math|<big|sum><rsub|c>>
  <math|e<rsup|d<rsup|T>*e<rsub|c>\<noplus\>+h<rsup|T>*U*e<rsub|c>>>.

  Note that we denote by <math|W<rsub|i>> the row <with|font-shape|italic|i>
  of matrix <with|font-shape|italic|W>, and by <math|W<rsup|i><rsub|>> the
  column <with|font-shape|italic|i> of matrix <with|font-shape|italic|W>.

  \;

  Valuable reference for this was given to us from
  [http://uai.sis.pitt.edu/papers/11/p463-louradour.pdf and,
  http://www.dmi.usherb.ca/~larocheh/publications/drbm-mitacs-poster.pdf].

  <with|font-series|bold|Learning>

  Contrastive divergence can be used to train the network, giving rise to the
  following update rules:

  \ 

  <\eqnarray*>
    <tformat|<table|<row|<cell|b<rprime|'> \ >|<cell|=>|<cell|b +
    \<lambda\><around*|(|v- v<rsub|sampled>|)>>>|<row|<cell|c<rprime|'>
    >|<cell|=>|<cell|c + \<lambda\><around*|(|h<rsub|\<sigma\>>-
    h<rsub|\<sigma\>sampled>|)>>>|<row|<cell|d<rprime|'> >|<cell|=>|<cell| d
    + \<lambda\><around*|(|y- y<rsub|sampled>|)>>>|<row|<cell| W<rprime|'>
    >|<cell|=>|<cell|W + \<lambda\><around*|(|h<rsub|\<sigma\>>*v<rsup|T>-h<rsub|\<sigma\>sampled>
    v<rsub|sampled><rsup|T>|)>>>|<row|<cell|U<rprime|'>>|<cell|=>|<cell| U +
    \<lambda\><around*|(|<around*|\<nobracket\>|h<rsub|\<sigma\>>*y<rsup|T>-h<rsub|\<sigma\>sampled>*y<rsub|sampled><rsup|T>|)>|\<nobracket\>>>>>>
  </eqnarray*>

  where we denote by <math|x<rprime|'>> the new value of parameter x.
  <math|v<rsub|sampled> >and <math|y<rsub|sampled> >are obtained by sampling
  the distributions in (5) and (6).\ 

  <\eqnarray*>
    <tformat|<table|<row|<cell|h<rsub|\<sigma\>>>|<cell|=>|<cell|\<sigma\><around*|(|c<rsub|j>+
    \ \<noplus\>\<noplus\>W<rsub|j>*v\<noplus\>+U<rsub|j>*y
    |)>>>|<row|<cell|h<rsub|\<sigma\>sampled>>|<cell|=>|<cell|\<sigma\><around*|(|c<rsub|j>+
    \ \<noplus\>\<noplus\>W<rsub|j>*v<rsub|sampled>\<noplus\>+U<rsub|j>*y<rsub|sampled>
    |)>>>>>
  </eqnarray*>

  \ <with|font-series|bold|Classification>

  <\equation*>
    p<around*|(|y=e<rsub|c><around*|\||v|\<nobracket\>>|)>=
    <frac|e<rsup|-F<around*|(|v\<nocomma\>,e<rsub|c>|)>>|<big|sum><rsub|d>e<rsup|-F<around*|(|v\<nocomma\>,e<rsub|d>|)>>>
  </equation*>

  where <math|F<around*|(|v\<nocomma\>,e<rsub|c>|)> >is the
  <with|font-shape|italic|free energy function>

  <\equation*>
    F<around*|(|v\<nocomma\>,e<rsub|c>|)>=
    -d<rsup|T>*y-<big|sum><rsup|H><rsub|j=1>
    <with|font-shape|italic|<with|font-shape|italic|s>><around*|(|c<rsub|j>+
    \ \<noplus\>\<noplus\>W<rsub|j>*v\<noplus\>+U<rsub|j>*y |)>\<nocomma\>
  </equation*>

  where <math|s<around*|(|x|)>= log<around*|(|1+e<rsup|x>|)> >

  \;

  The implementation of the Classification Boltzmann Machine can be found in

  <code*|ClassficationBoltzmannMachine.hs. >

  \;

  <with|font-series|bold|New method>

  Another method we have employed using Boltzmann Machines was created by us.
  We have never seen this approach used somewhere else. Instead of creating 2
  types of visible units, we use the simple Restricted Boltzmann Machine,
  with one type of visible units (and hence one single matrix). For each
  training vector we append the classification at the end. The classification
  is represented as a binary vector, either as above, by using
  <math|e<rsub|c>>, where c is the classification of the current pattern, or
  even in a more compressed manner, by creating the binary vector using the
  representation in base 2 of class c.\ 

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ <big-figure|
  \ \ \ \ \ \ \ \ \ \ \ \ \ <image|diagram3mid.png|350px|350px||>|Boltzmann
  machines according to our new method> \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 

  The training is done in the usual way, but with the complete vectors
  (actual pattern and classification). In our case, as different patterns
  ought to have different classifcations, we use as class the index of the
  pattern in the list (with removed duplicates) of training patterns.

  When a pattern needs to be matched to one of the training patterns for
  recognition, one uses the log probability (given using ...) to compute the
  probability of each of the classifications, and choses the one with maximum
  probability.

  \;

  <big-figure|<image|diagram4.png|622px|182px||>|The recognition process in
  the new Boltzmann machine.>

  \;

  As given in http://www.cs.toronto.edu/~hinton/absps/guideTR.pdf, the log
  probability is given by:

  <\equation*>
    <with|font-shape|italic|log><around*|(|class=c<around*|\||v|\<nobracket\>>|)>=
    <frac|e<rsup|-F<rsub|c><around*|(|v|)>>|<big|sum><rsub|c<rprime|'>>e<rsup|-F<rsub|c<rsup|><rprime|'>><around*|(|v|)>>>
  </equation*>

  <\equation*>
    F<around*|(|v|)>= -<big|sum><rsub|i\<in\>V>v<rsub|i>a<rsub|i>
    -<big|sum><rsub|j\<in\>H>log<around*|(|1 \<noplus\>+e<rsup|x<rsub|j>>|)>
  </equation*>

  \;

  where <math|x<rsub|j> =b<rsub|j>+<big|sum><rsub|i>w<rsub|i*j*<rsub|>>*v<rsub|j>>

  \;

  The implementation of this procedure can be found in
  <code*|RestrictedBoltzmannMachine.hs.>
</body>

<\initial>
  <\collection>
    <associate|language|british>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|2|?>>
    <associate|auto-3|<tuple|3|?>>
    <associate|auto-4|<tuple|4|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|figure>
      <tuple|normal|Restricted Boltzmann Machine|<pageref|auto-1>>
    </associate>
  </collection>
</auxiliary>