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
  [key, value1, value2] = line.split()
  value1 = float(value1)
  value2 = float(value2)
  key = round(float(key), 2)

  if key in vals:
  	vals[key][0].append(value1)
  	vals[key][1].append(value2)
  else:
  	vals[key] = ([value1], [value2])


print "key\tn\tmean origin\tstddev origin\tmean new\tstddev new"

for (key, (lst1, lst2)) in vals.iteritems():
	(mean1, stdv1) = meanstdv(lst1)
	(mean2, stdv2) = meanstdv(lst2)
	print "%s\t%d\t%f\t%f\t%f\t%f" %(key, len(lst1), mean1, stdv1, mean2, stdv2)

