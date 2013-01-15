#!/usr/bin/python

from __future__ import division
from sys import stdin

"""
Calculate mean and standard deviation of data x[]:
    mean = {\sum_i x_i \over n}
    std = sqrt(\sum_i (x_i - mean)^2 \over n-1)
    http://www.physics.rutgers.edu/~masud/computing/WPark_recipes_in_python.html
"""
def meanstdv(x):
    from math import sqrt
    n, mean, std = len(x), 0, 0
    for a in x:
	mean = mean + a
    mean = mean / float(n)
    for a in x:
	std = std + (a - mean)**2
    std = sqrt(std / float(n-1))
    return mean, std


vals = dict()

for line in stdin:
  if not line == "\n":
	  [key, value] = line.split()
	  value = round(float(value), 4)

  	  if key in vals:
  	  	vals[key].append(value)
  	  else:
	  	vals[key] = [value]


print "key\tn\tmean\tstddev"

for (key, lst) in vals.iteritems():
	(mean, stdv) = meanstdv(lst)
	print "%s\t%d\t%f\t%f" %(key, len(lst), mean, stdv)
