<TeXmacs|1.0.7.14>

<style|generic>

<\body>
  <with|font-series|bold|Errors in Hopfield networks>

  When a Hopfield network is trained using a list of patterns, it is desired
  that those patterns are attractors, which requires that they are fixed
  points. In case one of the patterns is not a fixed point, it is said that
  the network has an error.

  <em|>We will now derive the equations for errors in Hopfield networks.
  Firstly, we shall derive the probability of error for a pattern which was
  used for training, given that all the patterns were independent. Secondly,
  we will derive the equation of error for a super attractor: a pattern which
  was used to train the network, but which appeared in the training multiple
  times. The assumption for this derivation is that the other training
  patterns are independent. In the context of attachment types, we can assume
  that the super attractor is the primary care giver, which was consistent in
  his/her behaviour, and that the other encounters of the child were
  independent.

  <with|font-series|bold|Stability of stored states, given independence of
  all stored patterns>

  Consider a pattern stored in the network
  <with|font-shape|italic|x<math|<rsup|k>>>. We want to find out the
  probability of error for that pattern: how probable is that given that
  <with|font-shape|italic|x<math|<rsup|k>>> is stored in the network it is
  not a fixed point?

  Given the update formulae for neuron <math|i > in pattern
  <with|font-shape|italic|k > <math|<left|(>*1<left|)>> and the formulae for
  the weights, according to the training of the network (<math|2>):

  <\equation*>
    h<rsub|i><rsup|k> = <big|sum><rsup|N><rsub|j =1>w<rsub|ij >
    \ x<rsub|j><rsup|k>\ 
  </equation*>

  \ 

  <\equation>
    w<rsub|ij> = <frac|1|N><big|sum><rsup|p><rsub|l =
    1>x<rsup|l><rsub|i<rsup|>> \ x<rsub|j><rsup|l>
  </equation>

  we obtain by substitution:

  <\equation*>
    h<rsub|i><rsup|k> = <big|sum><rsup|N><rsub|j
    =1><rsub|><around*|(|<rsup|><frac|1|N><big|sum><rsup|p><rsub|l =1>
    x<rsup|l><rsub|i<rsup|>> \ x<rsup|l><rsub|j>|)><rsub|>\<cdot\>
    x<rsub|j><rsup|k> = <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l =1> x<rsup|l><rsub|i<rsup|>>
    x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j> =
  </equation*>

  <\equation*>
    = <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><rsup|>x<rsup|k><rsub|i<rsup|>>
    x<rsup|k><rsub|j>*x<rsup|k><rsub|<rsup|>j> \<noplus\>\<noplus\> +
    <frac|1|N> <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    =1\<nocomma\>,l \<neq\>k>x<rsup|l><rsub|i<rsup|>>
    \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j>
  </equation*>

  <\equation*>
    = <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><rsup|>x<rsup|k><rsub|i<rsup|>> \ + <frac|1|N>
    <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    =1\<nocomma\>,l \<neq\>k>x<rsup|l><rsub|i<rsup|>>
    \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j> = x<rsup|k><rsub|i<rsup|>>
    \ + <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l =1\<nocomma\>,l
    \<neq\>k>x<rsup|l><rsub|i<rsup|>> \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j>
  </equation*>

  <\equation*>
    \;
  </equation*>

  <math|The new value of> neuron i <math|x<rsup|k <rprime|'>><rsub|i > >is
  given by the sign of <math|h<rsub|i><rsup|k> >, so:

  \;

  \;

  <\equation>
    h<rsup|k><rsub|i > \ x<rsup|k ><rsub|i
    ><choice|<tformat|<table|<row|<cell|\<geqslant\> 0 \<nocomma\>
    \<nocomma\>\<nocomma\>if x<rsup|k><rsub|i><rprime|'> =
    x<rsup|k><rsub|i>>>|<row|<cell|\<less\>0 otherwise>>>>><rsup|><rsub| >
    \<Rightarrow\> -<rsub|>h<rsup|k><rsub|i > \ x<rsup|k ><rsub|i
    ><choice|<tformat|<table|<row|<cell|\<leqslant\> 0 \<nocomma\>
    \<nocomma\>\<nocomma\>if x<rsup|k><rsub|i><rprime|'> =
    x<rsup|k><rsub|i>>>|<row|<cell|\<gtr\> 0 otherwise>>>>>
  </equation>

  <\equation>
    - h<rsup|k><rsub|i>*x<rsup|k><rsub|<rsup|>i> =- x<rsup|k><rsub|i<rsup|>>
    \ x<rsup|k><rsub|i<rsup|>> - x<rsup|k><rsub|i<rsup|>>\<cdot\><around*|(|<frac|1|N>
    <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    =1\<nocomma\>,l \<neq\>k>x<rsup|l><rsub|i<rsup|>>
    x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j>|)>= 1
    \ <stack|<tformat|<table|<row|<cell|-
    <wide*|<frac|1|N><big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l =1\<nocomma\>,l
    \<neq\>k>x<rsup|l><rsub|i<rsup|>> \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j>*x<rsup|k><rsub|i><rsub|>|\<wide-underbrace\>><rsub|C<rsup|k><rsub|i>>>>>>>
  </equation>

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 

  From (3) and (<math|>4) we conclude that we get an error if
  <math|C<rsub|i><rsup|k> \<gtr\> 1> , so the probability of getting an error
  is the given by the probability of \ <math|C<rsub|i><rsup|k> \<gtr\> 1>.

  We model <math|C<rsub|i><rsup|k> > as follows. Each
  <math|x<rsup|l><rsub|i<rsup|>> \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j>*x<rsup|k>>
  can be modelled as a sample drawn from a random variable X with
  <math|<em|E(X) = 0 >> and <math|Var(X) = E(x<rsup|2>) -E(x) <rsup|2> =1.>

  By using the central limit theorem and by approximating <math|N <left|(>p >
  - <math|1>) to <math|N*p> :

  <\equation*>
    <frac|1|N*p><big|sum><rsub|i=1><rsup|N*p>X<rsub|i> \<sim\>N
    <around*|(|0\<nocomma\>, <frac|1|N p>|)>
  </equation*>

  \;

  Thus,\ 

  <\equation*>
    <frac|1|N><big|sum><rsub|i=1><rsup|N*p>X<rsub|i> \<sim\> N
    <around*|(|0\<nocomma\>, <frac|p|N>|)>
  </equation*>

  <\equation*>
    C<rsup|k><rsub|i > \<sim\> N <around*|(|0\<nocomma\>, <frac|p|N>|)>
  </equation*>

  By denoting <math|<frac|p|N>> with <math|\<sigma\>:>

  <\equation*>
    P<around*|(|C<rsup|k><rsub|i > \<gtr\> 1|)> \<simeq\>
    <frac|1|<sqrt|2\<mathpi\>>\<sigma\>><big|int><rsup|\<infty\>><rsub|1>e<rsup|-
    <frac|x<rsup|2>|2 \<sigma\><rsup|2>>> dx =
    <frac|1|<sqrt|2\<mathpi\>>\<sigma\>><big|int><rsup|\<infty\>><rsub|<frac|1|<sqrt|2>
    \<sigma\>>> e<rsup|-y<rsup|2>> <sqrt|2 >\<sigma\>dy =\ 
  </equation*>

  <\equation*>
    = <frac|1|<sqrt|\<mathpi\>>><big|int><rsup|\<infty\>><rsub|<frac|1|<sqrt|2>
    \<sigma\>>> e<rsup|- y<rsup|2>> dy= <frac|1|<sqrt|\<mathpi\>>><around*|(|<big|int><rsup|\<infty\>><rsub|0>
    e<rsup|-y<rsup|2>> dy - <big|int><rsup|<frac|1|<sqrt|2>
    \<sigma\>>><rsub|0> e<rsup|- y<rsup|2>> dy |)>=
  </equation*>

  <\equation*>
    = <frac|1|<sqrt|\<mathpi\>>><big|int><rsup|\<infty\>><rsub|0> e<rsup|-
    y<rsup|2>> dy - \ <frac|1|<sqrt|\<mathpi\>>><big|int><rsup|<frac|1|<sqrt|2>
    \<sigma\>>><rsub|0> e<rsup|- y<rsup|2>> dy = <frac|1|<sqrt|\<mathpi\>>>
    \<ast\><frac|<sqrt|\<pi\>>|2> \<noplus\>-
    <frac|1|2>erf<around*|(|<frac|1|<sqrt|2 \<sigma\>>>|)>=
  </equation*>

  <\equation*>
    = <frac|1|2><around*|(|1 \<noplus\>\<noplus\>-
    erf<around*|(|<frac|1|<sqrt|2 \<sigma\>>>|)>|)> =
    \ <frac|1|2><around*|(|1 \<noplus\>\<noplus\>-
    <with|font-shape|italic|erf><around*|(|<sqrt|<frac|N|2p>>|)>|)>\ 
  </equation*>

  <\equation*>
    where err x = <frac|1|<sqrt|2\<pi\>>><big|int><rsup|x><rsub|0>e<rsup|-x<rsup|2>
    ><rsub|<rsup|<rsup|>>>dx\ 
  </equation*>

  \;

  <\equation*>
    \;
  </equation*>

  <\equation*>
    \;
  </equation*>

  <with|font-series|bold|Stability of a super attractor, given independence of
  all other stored patterns>

  \;

  By following the above way of reasoning, we compute the probability of
  error for a super attractor.

  Given that the super attractor has degree d (it appears d times in the list
  of patterns used to train the network), we compute the probability that it
  is not a fixed point.\ 

  Let <math|x<rsup|k><rsub|>> be the supper attractor with degree d.

  Let S = <math|<left|{> i <left|\|> ><math|x<rsup|i><rsub|> = x<rsup|k>> , i
  <em|<math|\<in\><around*|{|1 \<ldots\>p|}> >>}, be the set of indexes of
  occurences of the supper attractor <math|x<rsup|k><rsub|>>.

  <math|We assumed that the super attractor has degree
  d\<nocomma\>\<nocomma\>\<nocomma\> \<Rightarrow\> <left|\|> S <left|\|> =
  d\<nosymbol\> >.

  \;

  <\equation*>
    h<rsub|i><rsup|k> = <big|sum><rsup|N><rsub|j
    =1><rsub|><around*|(|<rsup|><frac|1|N><big|sum><rsup|p><rsub|l =1>
    x<rsup|l><rsub|i<rsup|>> \ x<rsup|l><rsub|j>|)><rsub|> \<cdot\>
    x<rsub|j><rsup|k> = <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l =1> x<rsup|l><rsub|i<rsup|>>
    \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j> =
  </equation*>

  <\eqnarray*>
    <tformat|<table|<row|<cell| <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><rsup|><big|sum><rsub|l \<in\>S><rsup|p>x<rsup|k><rsub|i<rsup|>>
    \ x<rsup|k><rsub|j>*x<rsup|l><rsub|<rsup|>j> \<noplus\>\<noplus\> +
    <frac|1|N> <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    \<nin\>S>x<rsup|l><rsub|i<rsup|>> \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j>
    >|<cell|=>|<cell| <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><rsup|><big|sum><rsub|l \<in\>S><rsup|p>x<rsup|k><rsub|i<rsup|>>
    \ x<rsup|k><rsub|j>*x<rsup|k><rsub|<rsup|>j> \<noplus\>\<noplus\> +
    <frac|1|N> <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    \<nin\>S>x<rsup|l><rsub|i<rsup|>> \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j>
    = >>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<frac|1|N>
    <big|sum><rsup|N><rsub|j =1><rsub|><rsup|><big|sum><rsub|l
    \<in\>S><rsup|p>x<rsup|k><rsub|i<rsup|>> \<noplus\>+\<noplus\>
    \ <frac|1|N> <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    \<nin\>S>x<rsup|l><rsub|i<rsup|>> \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j>
    \ >|<cell|=>|<cell| <frac|1|N><rsub|><rsup|> N d x<rsup|k><rsub|i<rsup|>>
    \<noplus\>+<frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l \<nin\>S>x<rsup|l><rsub|i<rsup|>>
    \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j> = >>>>
  </eqnarray*>

  <\equation*>
    = d x<rsup|k><rsub|i<rsup|>> \<noplus\>+<frac|1|N>
    <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    \<nin\>S>x<rsup|l><rsub|i<rsup|>> \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j>
  </equation*>

  Thus

  <\equation*>
    -h<rsup|k><rsub|i> x<rsup|k><rsub|i>= - d
    x<rsub|i><rsup|k>*x<rsub|i><rsup|k> - <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l \<nin\>S>x<rsup|l><rsub|i<rsup|>>
    \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j> x<rsup|k><rsub|i<rsup|>>=\ 
  </equation*>

  <\equation*>
    - d \ -<wide*| <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l \<nin\>S>x<rsup|l><rsub|i<rsup|>>
    \ x<rsup|l><rsub|j>*x<rsup|k><rsub|<rsup|>j> x<rsup|k><rsub|i<rsup|>>
    |\<wide-underbrace\>><rsub|S<rsup|k><rsub|i>>
  </equation*>

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 

  In order to get an error, <math|minus h<rsup|k><rsub|i>*x<rsup|k><rsub|i>\<leqslant\>0
  \<nocomma\>> , so <math| S<rsup|k><rsub|i>>
  <math|\<geqslant\>d\<nosymbol\>>. By using the same reasoning as before,\ 

  <\equation*>
    <frac|1|N*<around*|(|p- d|)>><big|sum><rsub|i=1><rsup|N*<around*|(|p-
    d|)>>X<rsub|i> \<sim\>N <around*|(|0\<nocomma\>, <frac|1|N <around*|(|p -
    d|)>>|)>
  </equation*>

  <\equation*>
    <frac|1|N><big|sum><rsub|i=1><rsup|N*<around*|(|p-d|)>>X<rsub|i> \<sim\>N
    <around*|(|0\<nocomma\>, <frac|p - d|N >|)>
  </equation*>

  <\equation*>
    S<rsup|k><rsub|i >\<sim\>N <around*|(|0\<nocomma\>, <frac|p -d|N >|)>
  </equation*>

  By denoting <math|<frac|p -d|N>> with <math|\<sigma\>:>

  <\equation*>
    P<around*|(|S<rsup|k><rsub|i > \<gtr\> d|)> \<simeq\>
    <frac|1|<sqrt|2\<mathpi\>>\<sigma\>><big|int><rsup|\<infty\>><rsub|d>e<rsup|-
    <frac|x<rsup|2>|2 \<sigma\><rsup|2>>> dx =
    <frac|1|<sqrt|2\<mathpi\>>\<sigma\>><big|int><rsup|\<infty\>><rsub|<frac|d|<sqrt|2>
    \<sigma\>>> e<rsup|-y<rsup|2>> <sqrt|2 >\<sigma\>dy =\ 
  </equation*>

  <\equation*>
    = <frac|1|<sqrt|\<mathpi\>>><big|int><rsup|\<infty\>><rsub|<frac|d|<sqrt|2>
    \<sigma\>>> e<rsup|- y<rsup|2>> dy= <frac|1|<sqrt|\<mathpi\>>><around*|(|<big|int><rsup|\<infty\>><rsub|0>
    e<rsup|- y<rsup|2>> dy - <big|int><rsup|<frac|d|<sqrt|2>
    \<sigma\>>><rsub|0> e<rsup|- y<rsup|2>> dy |)>=
  </equation*>

  <\equation*>
    = <frac|1|<sqrt|\<mathpi\>>><big|int><rsup|\<infty\>><rsub|0>
    e<rsup|-y<rsup|2>> dy - \ <frac|1|<sqrt|\<mathpi\>>><big|int><rsup|<frac|d|<sqrt|2>
    \<sigma\>>><rsub|0> e<rsup|-y<rsup|2>> dy = <frac|1|<sqrt|\<mathpi\>>>
    <frac|<sqrt|\<pi\>>|2> \<noplus\>- <frac|1|2>erf<around*|(|<frac|d|<sqrt|2
    \<sigma\>>>|)>=
  </equation*>

  <\equation*>
    = <frac|1|2><around*|(|1 \<noplus\>\<noplus\>-
    erf<around*|(|<frac|1|<sqrt|2 \<sigma\>>>|)>|)> =
    \ <frac|1|2><around*|(|1 \<noplus\>\<noplus\>-
    <with|font-shape|italic|erf><around*|(|<sqrt|<frac|N|2<around*|(|p -
    d|)>>>|)>|)>\ 
  </equation*>

  \;

  <with|font-series|bold|Computing network parameters given maximum accepted
  error>

  Assuming all training patterns are independent, we can use the above
  results to determine the minimum number of neurons which should be used for
  the network given the number of training patterns we want to store. In the
  case of image recognition, this can provide to be useful as it can give a
  guideline towards how to resize the images in order to bring them to the
  same size (patterns which are stored in a Hopfield network need to have
  equal lengths). It also enables us to compute the capacity of a network of
  given size, in order to minimise errors.

  Given p and N, we deduced the probability of error for the network. Thus,
  conversely, we can compute the maximum ratio of p and N to ensure that the
  probability of error does not exceed a maximum accepted error probability.

  <\equation*>
    P<rsub|error >= <frac|1|2>*<around*|(|1 \<noplus\>\<noplus\>-
    <with|font-shape|italic|erf><around*|(|<sqrt|<frac|N|2p>>|)>|)>\ 
  </equation*>

  <\eqnarray*>
    <tformat|<table|<row|<cell|\<Rightarrow\> erf<sqrt|<frac|N|2p>> = 1 -2
    P<rsub|error >>|<cell|\<Rightarrow\>>|<cell|<sqrt|<frac|N|2p>> = inverf
    <around*|(|1 - 2*P<rsub|error >|)>>>>>
  </eqnarray*>

  <\equation*>
    \<Rightarrow\> <frac|N|p> =2*<around*|(| inverf <around*|(|1 -
    2*P<rsub|error >|)>|)><rsup|2>
  </equation*>

  <\eqnarray*>
    <tformat|<table|<row|<cell|<frac|p|N>=<frac|1|2*<around*|(| inverf
    <around*|(|1 - 2*P<rsub|error >|)>|)><rsup|2>>>|<cell|and>|<cell|N =2*p
    <around*|(| inverf <around*|(|1 - 2*P<rsub|error >|)>|)><rsup|2>>>>>
  </eqnarray*>

  where inverf is the inverse of the erf function.

  <with|font-series|bold|Measurements using the error of a network>
  <with|font-series|bold|for independent patterns>

  We will now give the reader an idea of how one should accommodate a fixed
  number of patterns depending on the error accepted by the application, by
  creating networks of appropriate size.

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 

  <\big-table|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lborder|1px>|<cwith|1|-1|1|-1|cell-rborder|1px>|<cwith|1|-1|1|-1|cell-bborder|1px>|<cwith|1|-1|1|-1|cell-tborder|1px>|<cwith|1|-1|3|3|cell-background|<pattern|/usr/share/texmacs/TeXmacs/misc/patterns/metal-brushed-light.png||>>|<table|<row|<cell|<with|font-series|bold|p>>|<cell|<with|font-series|bold|error>>|<cell|<with|font-series|bold|N>>>|<row|<cell|100>|<cell|0.1>|<cell|165>>|<row|<cell|100>|<cell|0.01>|<cell|542>>|<row|<cell|100>|<cell|0.05>|<cell|271>>|<row|<cell|100>|<cell|0.001>|<cell|955>>>>>>
    Getting the minimum numbers of neurons required by the network by fixing
    the error

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ and the
    number of patterns used to train the network.
  </big-table>

  The following table describes the capacity of the network in terms of the
  error accepted, by fixing the size of the network.

  <big-table|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lborder|1px>|<cwith|1|-1|1|-1|cell-rborder|1px>|<cwith|1|-1|1|-1|cell-bborder|1px>|<cwith|1|-1|1|-1|cell-tborder|1px>|<cwith|1|-1|3|3|cell-background|<pattern|/usr/share/texmacs/TeXmacs/misc/patterns/ridged-brushed-dark.png||>>|<cwith|5|5|3|3|cell-background|<pattern|/usr/share/texmacs/TeXmacs/misc/patterns/ridged-brushed-medium.png||>>|<cwith|1|4|3|3|cell-background|<pattern|/usr/share/texmacs/TeXmacs/misc/patterns/ridged-brushed-medium.png||>>|<table|<row|<cell|<with|font-series|bold|N>>|<cell|<with|font-series|bold|error>>|<cell|<with|font-series|bold|p>>>|<row|<cell|1000>|<cell|0.1>|<cell|608>>|<row|<cell|1000>|<cell|0.01>|<cell|184>>|<row|<cell|1000>|<cell|0.05>|<cell|359>>|<row|<cell|1000>|<cell|0.001>|<cell|104>>>>><with|font-series|bold|>|Getting
  the capacity of a network by fixing the error and the size of a network.>

  The above results can be reproduced by using the Analysis module. For
  example, by using <code*|ghci>:

  <kbd|<code*|>*Analysis \<gtr\> computeErrorSuperAttractor 10 100>

  The first argument of the function is <math|p> (the number of training
  patterns), and the second one is <math|N> (the size of the network). A
  similar function can be used for a given network
  <code*|computeErrorSuperAttractor>, that given a network computes the
  probability of error of the super attractor. The caller has to ensure that
  training patterns are independent in order for the computation to be
  correct.

  <with|font-series|bold|Decreasing the error by using one super attractor>

  Above we described the derivation for obtaining the error of a super
  attractor, given its degree. We remind the reader that we did this under
  the assumption that all other training patterns are independent. We will now
  show how the error of a pattern decreases if it is made a super attractor,
  by varying the number of times it is presented to the network during
  training.

  \;

  <small-table|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lborder|1px>|<cwith|1|-1|1|-1|cell-rborder|1px>|<cwith|1|-1|1|-1|cell-bborder|1px>|<cwith|1|-1|1|-1|cell-tborder|1px>|<cwith|1|-1|4|4|cell-background|<pattern|/usr/share/texmacs/TeXmacs/misc/patterns/ridged-brushed-medium.png||>>|<table|<row|<cell|<with|font-series|bold|degree>>|<cell|<with|font-series|bold|p>>|<cell|<with|font-series|bold|N>>|<cell|<with|font-series|bold|error>>>|<row|<cell|1>|<cell|20>|<cell|100>|<cell|1.08
  *<math|10<rsup|-2>>>>|<row|<cell|2>|<cell|20>|<cell|100>|<cell|7.64*<math|10<rsup|-3>>>>|<row|<cell|5>|<cell|20>|<cell|100>|<cell|4.91*<math|10<rsup|-3>>>>|<row|<cell|8>|<cell|20>|<cell|100>|<cell|<math|1.95\<ast\>10<rsup|-3>>>>|<row|<cell|11>|<cell|20>|<cell|100>|<cell|<math|4.29
  *\<ast\>10<rsup|-4>>>>|<row|<cell|15>|<cell|20>|<cell|100>|<cell|<math|3.87\<ast\>10<rsup|-6>>>>>>>|>
  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ <small-table|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lborder|1px>|<cwith|1|-1|1|-1|cell-rborder|1px>|<cwith|1|-1|1|-1|cell-bborder|1px>|<cwith|1|-1|1|-1|cell-tborder|1px>|<cwith|1|-1|4|4|cell-background|<pattern|/usr/share/texmacs/TeXmacs/misc/patterns/ridged-brushed-medium.png||>>|<table|<row|<cell|<with|font-series|bold|degree>>|<cell|<with|font-series|bold|p>>|<cell|<with|font-series|bold|N>>|<cell|<with|font-series|bold|error>>>|<row|<cell|1>|<cell|50>|<cell|100>|<cell|<math|7.65
  \<ast\>10<rsup|<rsup|-2>>>>>|<row|<cell|5>|<cell|50>|<cell|100>|<cell|6.80*<math|10<rsup|-2>>>>|<row|<cell|10>|<cell|50>|<cell|100>|<cell|5.69*10<math|<rsup|-2>>>>|<row|<cell|20>|<cell|50>|<cell|100>|<cell|3.39*<math|10<rsup|-2>>>>|<row|<cell|30>|<cell|50>|<cell|100>|<cell|<math|1.26\<ast\>10<rsup|-2>>>>|<row|<cell|40>|<cell|50>|<cell|100>|<cell|<math|7.82\<ast\>10<rsup|-4>>>>>>>|>

  Note that in the second table we are intentionally stressing the network so
  that we can notice how super attractors also affect the capacity of the
  network.

  The above results can be reproduced by using the Analysis module. For
  example, by using <code*|ghci>:

  <kbd|<code*|>*Analysis \<gtr\> computeErrorSuperAttractorNumbers 40 50 100>

  The first argument of the function is the degree of the attractor, the
  second one is <math|p> and the third one is <math|N>. A similar function
  can be used for a given network <code*|computeErrorSuperAttractor>, that
  given a network computes the probability of error of the super attractor.
  The caller of the function has to ensure that the training patterns contain
  a super attractor and that the other patterns are independent.\ 

  \;
</body>

<\initial>
  <\collection>
    <associate|font-base-size|10>
    <associate|language|british>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|4>>
    <associate|auto-2|<tuple|2|4>>
    <associate|auto-3|<tuple|3|5>>
    <associate|auto-4|<tuple|4|5>>
    <associate|footnote-1|<tuple|1|?>>
    <associate|footnote-2|<tuple|2|?>>
    <associate|footnr-1|<tuple|1|?>>
    <associate|footnr-2|<tuple|2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <\tuple|normal>
        Getting the minimum numbers of neurons required by the network by
        fixing the error

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ and the
        number of patterns used to train the network.
      </tuple|<pageref|auto-1>>

      <tuple|normal|Getting the capacity of a network by fixing the error and
      the size of a network.|<pageref|auto-2>>

      <tuple|normal||<pageref|auto-3>>

      <tuple|normal||<pageref|auto-4>>
    </associate>
  </collection>
</auxiliary>