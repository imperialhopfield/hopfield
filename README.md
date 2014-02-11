Attractor Neural Networks for Modelling Associative Memory
==========================================================

[![Build Status](https://travis-ci.org/imperialhopfield/hopfield.png)](https://travis-ci.org/imperialhopfield/hopfield)

A third year Computing group project at [Imperial College London](http://www3.imperial.ac.uk/computing/), supervised by [Prof. Abbas Edalat](https://www.doc.ic.ac.uk/~ae/).

Get it from [Hackage](http://hackage.haskell.org/package/hopfield) via: `cabal install hopfield`.

Our **report**: <https://github.com/imperialhopfield/hopfield/raw/master/report/report.pdf>


Features
--------

This project implements (in Haskell):

* [Hopfield Networks](src/Hopfield/Hopfield.hs)
* [Clusters](src/Hopfield/Clusters.hs) and [Super Attractors](src/Hopfield/SuperAttractors.hs)
* The [Restricted Boltzmann Machine](src/Hopfield/Boltzmann/RestrictedBoltzmannMachine.hs)
* A [Boltzmann Machine for classification](src/Hopfield/Boltzmann/ClassificationBoltzmannMachine.hs)

and comes with a range of experiments to evaluate their properties.


License
-------

MIT licensed. We appreciate a mention if our code or results were useful for you.

Written by:

* Mihaela Rosca
* Wael Al Jisihi
* Niklas Hambüchen
* Razvan Marinescu
* Lukasz Severyn
