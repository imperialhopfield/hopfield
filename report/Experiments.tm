<TeXmacs|1.0.7.14>

<style|generic>

<\body>
  <with|font-series|bold|Comparing Storkey and Hebbian learning>

  When training a Hopfield network, we have employed two types of learning:
  the first one is the hebbian learning, while the second one is Storkey
  learning. The advatage of the latter is that it increases the capacity of
  the network and the basin of attraction of the clusters. We will now
  explain some experiments we did with both types of learning in order to see
  how the learning type affects the basin of attraction for clusters.\ 

  We will now show how the learning determines the basin of attraction of a
  Gaussian distributed cluster. The generation was done using the T2 method
  described TODO in section.\ 

  <big-table|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lborder|1px>|<cwith|1|-1|1|-1|cell-rborder|1px>|<cwith|1|-1|1|-1|cell-bborder|1px>|<cwith|1|-1|1|-1|cell-tborder|1px>|<table|<row|<cell|<with|font-series|bold|Learning>>|<cell|N>|<cell|Cluster
  size>|<cell|<math|\<mu\>>>|<cell|<math|\<sigma\>>>|<cell|Average size of
  basin of attraction>>|<row|<cell|Hebbian>|<cell|50>|<cell|2>|<cell|25>|<cell|5>|<cell|10.0>>|<row|<cell|Storkey>|<cell|50>|<cell|2>|<cell|25>|<cell|5>|<cell|18.0>>|<row|<cell|Hebbian>|<cell|50>|<cell|5>|<cell|25>|<cell|5>|<cell|4.4>>|<row|<cell|Storkey>|<cell|50>|<cell|5>|<cell|25>|<cell|5>|<cell|4.8>>|<row|<cell|Hebbian>|<cell|50>|<cell|4>|<cell|25>|<cell|10>|<cell|5.75>>|<row|<cell|Storkey>|<cell|50>|<cell|4>|<cell|25>|<cell|10>|<cell|8.0>>|<row|<cell|Hebbian>|<cell|50>|<cell|4>|<cell|25>|<cell|5>|<cell|1.5>>|<row|<cell|Storkey>|<cell|50>|<cell|4>|<cell|25>|<cell|5>|<cell|4>>|<row|<cell|Hebbian>|<cell|50>|<cell|6>|<cell|25>|<cell|5>|<cell|>>|<row|<cell|Storkey>|<cell|50>|<cell|6>|<cell|25>|<cell|5>|<cell|>>|<row|<cell|Hebbian>|<cell|50>|<cell|6>|<cell|25>|<cell|10>|<cell|4.45>>|<row|<cell|Storkey>|<cell|50>|<cell|6>|<cell|25>|<cell|10>|<cell|5.61>>>>>|>

  \;

  The results confirm what the mathematical theory showed us: Storkey
  learning increases the average basin of attraction. We must note that this
  does not come without a price: training the network using Storkey learning
  slowed down our experiments, as it is more expensive.
</body>

<\initial>
  <\collection>
    <associate|language|british>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal||<pageref|auto-1>>
    </associate>
  </collection>
</auxiliary>