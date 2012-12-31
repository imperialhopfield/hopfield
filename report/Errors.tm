<TeXmacs|1.0.7.14>

<style|generic>

<\body>
  <with|font-series|bold|Errors in Hopfield networks>

  When a Hopfield network is trained using a list of patterns, it is desired
  that those patterns are attractors, which requires that they are fixed
  points. In case one of the patterns is not a fixed point, it is said that
  the network has an error.

  <em|>We will now derive the equations for errors in Hopfield networks.
  Firstly, we shall derive the probabilty of error for a pattern which was
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
  <with|font-shape|italic|k > <math|<left|(>*\<star\><left|)>> and the
  formulae for the weights, according to the training of the network
  (<math|\<star\>\<star\>>):

  <\equation>
    h<rsub|i><rsup|k> = <big|sum><rsup|N><rsub|j =1>w<rsub|ij > \<ast\>
    x<rsub|j><rsup|k>\ 
  </equation>

  \ 

  <\equation>
    w<rsub|ij> = <frac|1|N><big|sum><rsup|p><rsub|l =
    1>x<rsup|l><rsub|i<rsup|>> \<ast\> x<rsub|j><rsup|l>
  </equation>

  we obtain by substitution:

  <\equation*>
    h<rsub|i><rsup|k> = <big|sum><rsup|N><rsub|j
    =1><rsub|><around*|(|<rsup|><frac|1|N><big|sum><rsup|p><rsub|l =1>
    x<rsup|l><rsub|i<rsup|>> \<ast\> x<rsup|l><rsub|j>|)><rsub|> \<ast\>
    x<rsub|j><rsup|k> = <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l =1> x<rsup|l><rsub|i<rsup|>> \<ast\>
    x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j> =
  </equation*>

  <\equation*>
    = <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><rsup|>x<rsup|k><rsub|i<rsup|>> \<ast\>
    x<rsup|k><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j> \<noplus\>\<noplus\> +
    <frac|1|N> <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    =1\<nocomma\>,l \<neq\>k>x<rsup|l><rsub|i<rsup|>> \<ast\>
    x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j>
  </equation*>

  <\equation*>
    = <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><rsup|>x<rsup|k><rsub|i<rsup|>> \ + <frac|1|N>
    <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    =1\<nocomma\>,l \<neq\>k>x<rsup|l><rsub|i<rsup|>> \<ast\>
    x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j> =
    x<rsup|k><rsub|i<rsup|>> \ + <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l =1\<nocomma\>,l
    \<neq\>k>x<rsup|l><rsub|i<rsup|>> \<ast\>
    x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j>\ 
  </equation*>

  <\equation*>
    \;
  </equation*>

  <math|The new value of> neuron i <math|x<rsup|k <rprime|'>><rsub|i > >is
  given by the sign of <math|h<rsub|i><rsup|k> >, so:

  \;

  \;

  <\equation>
    h<rsup|k><rsub|i > \<ast\> x<rsup|k ><rsub|i
    ><choice|<tformat|<table|<row|<cell|\<geqslant\> 0 \<nocomma\>
    \<nocomma\>\<nocomma\>if x<rsup|k><rsub|i><rprime|'> =
    x<rsup|k><rsub|i>>>|<row|<cell|\<less\>0 otherwise>>>>><rsup|><rsub| >
    \<Rightarrow\> -<rsub|>h<rsup|k><rsub|i > \<ast\> x<rsup|k ><rsub|i
    ><choice|<tformat|<table|<row|<cell|\<leqslant\> 0 \<nocomma\>
    \<nocomma\>\<nocomma\>if x<rsup|k><rsub|i><rprime|'> =
    x<rsup|k><rsub|i>>>|<row|<cell|\<gtr\> 0 otherwise>>>>>
  </equation>

  <\equation>
    - h<rsup|k><rsub|i>\<ast\>x<rsup|k><rsub|<rsup|>i> =-
    x<rsup|k><rsub|i<rsup|>> \ x<rsup|k><rsub|i<rsup|>> -
    x<rsup|k><rsub|i<rsup|>>\<ast\> <around*|(|<frac|1|N>
    <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    =1\<nocomma\>,l \<neq\>k>x<rsup|l><rsub|i<rsup|>> \<ast\>
    x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j>|)>= 1
    \ <stack|<tformat|<table|<row|<cell|- <frac|1|N><wide*|<around|(|
    <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    =1\<nocomma\>,l \<neq\>k>x<rsup|l><rsub|i<rsup|>> \<ast\>
    x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j>\<ast\>x<rsup|k><rsub|i>|)>|\<wide-underbrace\>>>>>>>
  </equation>

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ <math|C<rsup|k><rsub|i>>
  \ \ \ \ \ \ 

  From (3) and (<math|>4) we conclude that we get an error if
  <math|C<rsub|i><rsup|k> \<gtr\> 1> , so the probability of getting an error
  is the given by the probability of \ <math|C<rsub|i><rsup|k> \<gtr\> 1>.

  We model <math|C<rsub|i><rsup|k> > as follows. Each
  <math|x<rsup|l><rsub|i<rsup|>> \<ast\> x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j>\<ast\>x<rsup|k><rsub|i>
  > can be model as a sample drawn from a random variable X with
  <math|<em|E(X) = 0 >> and <math|Var(X) = E(x<rsup|2>) -E(x) <rsup|2> =1.>

  By using the central limit theorem and by approximating <math|N
  \<ast\><left|(>p > - <math|1>) to <math|N\<ast\>p> :

  <\equation*>
    <frac|1|N\<ast\>p><big|sum><rsub|i=1><rsup|N*\<ast\>p>X<rsub|i> \<sim\>N
    <around*|(|0\<nocomma\>, <frac|1|N \<ast\>p>|)>
  </equation*>

  \;

  Thus,\ 

  <\equation*>
    <frac|1|N><big|sum><rsub|i=1><rsup|N*\<ast\>p>X<rsub|i> \<sim\> N
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

  <with|font-series|bold|Stability of a super attractor, given indpendence of
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
    x<rsup|l><rsub|i<rsup|>> \<ast\> x<rsup|l><rsub|j>|)><rsub|> \<ast\>
    x<rsub|j><rsup|k> = <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l =1> x<rsup|l><rsub|i<rsup|>> \<ast\>
    x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j> =
  </equation*>

  <\eqnarray*>
    <tformat|<table|<row|<cell| <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><rsup|><big|sum><rsub|l \<in\>S><rsup|p>x<rsup|k><rsub|i<rsup|>>
    \<ast\> x<rsup|k><rsub|j>\<ast\>x<rsup|l><rsub|<rsup|>j>
    \<noplus\>\<noplus\> + <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l \<nin\>S>x<rsup|l><rsub|i<rsup|>>
    \<ast\> x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j>
    >|<cell|=>|<cell| <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><rsup|><big|sum><rsub|l \<in\>S><rsup|p>x<rsup|k><rsub|i<rsup|>>
    \<ast\> x<rsup|k><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j>
    \<noplus\>\<noplus\> + <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l \<nin\>S>x<rsup|l><rsub|i<rsup|>>
    \<ast\> x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j> =
    >>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<frac|1|N>
    <big|sum><rsup|N><rsub|j =1><rsub|><rsup|><big|sum><rsub|l
    \<in\>S><rsup|p>x<rsup|k><rsub|i<rsup|>> \<noplus\>+\<noplus\>
    \ <frac|1|N> <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    \<nin\>S>x<rsup|l><rsub|i<rsup|>> \<ast\>
    x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j> \ >|<cell|=>|<cell|
    <frac|1|N><rsub|><rsup|> N d x<rsup|k><rsub|i<rsup|>>
    \<noplus\>+<frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l \<nin\>S>x<rsup|l><rsub|i<rsup|>>
    \<ast\> x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j> = >>>>
  </eqnarray*>

  <\equation*>
    = d x<rsup|k><rsub|i<rsup|>> \<noplus\>+<frac|1|N>
    <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    \<nin\>S>x<rsup|l><rsub|i<rsup|>> \<ast\>
    x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j>\ 
  </equation*>

  Thus

  <\equation*>
    -h<rsup|k><rsub|i> \<ast\>x<rsup|k><rsub|i>= - d
    \<ast\>x<rsub|i><rsup|k>\<ast\>x<rsub|i><rsup|k> - <frac|1|N>
    <big|sum><rsup|N><rsub|j =1><rsub|><big|sum><rsup|p><rsub|l
    \<nin\>S>x<rsup|l><rsub|i<rsup|>> \<ast\>
    x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j>
    \<ast\>x<rsup|k><rsub|i<rsup|>>=\ 
  </equation*>

  <\equation*>
    - d \ -<wide*| <frac|1|N> <big|sum><rsup|N><rsub|j
    =1><rsub|><big|sum><rsup|p><rsub|l \<nin\>S>x<rsup|l><rsub|i<rsup|>>
    \<ast\> x<rsup|l><rsub|j>\<ast\>x<rsup|k><rsub|<rsup|>j>
    \<ast\>x<rsup|k><rsub|i<rsup|>> |\<wide-underbrace\>>
  </equation*>

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ <math|S<rsup|k><rsub|i>>
  \ 

  In order to get an error, <math|minus h<rsup|k><rsub|i>
  \<ast\>x<rsup|k><rsub|i>\<leqslant\>0 \<nocomma\>> , so <math|
  S<rsup|k><rsub|i>> <math|\<geqslant\>d\<nosymbol\>>. By using the same
  reasoning as before,\ 

  <\equation*>
    <frac|1|N\<ast\><around*|(|p- d|)>><big|sum><rsub|i=1><rsup|N*\<ast\><around*|(|p-
    d|)>>X<rsub|i> \<sim\>N <around*|(|0\<nocomma\>, <frac|1|N
    \<ast\><around*|(|p - d|)>>|)>
  </equation*>

  <\equation*>
    <frac|1|N><big|sum><rsub|i=1><rsup|N*\<ast\><around*|(|p-d|)>>X<rsub|i>
    \<sim\>N <around*|(|0\<nocomma\>, <frac|p - d|N >|)>
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
    \<ast\><frac|<sqrt|\<pi\>>|2> \<noplus\>-
    <frac|1|2>erf<around*|(|<frac|d|<sqrt|2 \<sigma\>>>|)>=
  </equation*>

  <\equation*>
    = <frac|1|2><around*|(|1 \<noplus\>\<noplus\>-
    erf<around*|(|<frac|1|<sqrt|2 \<sigma\>>>|)>|)> =
    \ <frac|1|2><around*|(|1 \<noplus\>\<noplus\>-
    <with|font-shape|italic|erf><around*|(|<sqrt|<frac|N|2<around*|(|p -
    d|)>>>|)>|)>\ 
  </equation*>

  -- insert tables with errors here, given various values

  <with|font-series|bold|Computing network parameters given maximum accepted
  error>

  Assuming all training patterns are independent, we can use the above
  results to determine the minimum number of neurons which should be used for
  the network given the number of training patterns we want to store. In the
  case of image recognition, this can provide to be useful as it can give a
  guideline towards how to resize the images in order to bring them to the
  same size (patterns which are stored in a Hopfield network need to have
  equal lengths).

  Given p and N, we decuded the probability of error for the network. Thus,
  conversly, we can compute the maximum ratio of p and N to ensure that the
  probability of error does not exceed a maximum accepted error probability.

  <\equation*>
    P<rsub|error >= <frac|1|2><around*|(|1 \<noplus\>\<noplus\>-
    <with|font-shape|italic|erf><around*|(|<sqrt|<frac|N|2p>>|)>|)>\ 
  </equation*>

  <\eqnarray*>
    <tformat|<table|<row|<cell|\<Rightarrow\> erf<sqrt|<frac|N|2p>> = 1 -
    P<rsub|error >>|<cell|\<Rightarrow\>>|<cell|<sqrt|<frac|N|2p>> = inverr
    <around*|(|1 - P<rsub|error >|)>>>>>
  </eqnarray*>

  <\equation*>
    \<Rightarrow\> <frac|N|p> =2\<ast\><around*|(| inverr <around*|(|1 -
    P<rsub|error >|)>|)><rsup|2>
  </equation*>

  <\eqnarray*>
    <tformat|<table|<row|<cell|<frac|p|N>=<frac|1|2\<ast\><around*|(| inverr
    <around*|(|1 - P<rsub|error >|)>|)><rsup|2>>>|<cell|and>|<cell|N =2p
    \<ast\> <around*|(| inverr <around*|(|1 - P<rsub|error
    >|)>|)><rsup|2>>>>>
  </eqnarray*>

  <\equation*>
    \;
  </equation*>
</body>

<\initial>
  <\collection>
    <associate|font-base-size|10>
    <associate|language|british>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
  </collection>
</references>