
for i in xrange(10, 37):
  with open("pixel"+str(i)) as f:
    for line in f:
      print line
