import sys
import string

def format(filename):
  with open(filename, 'r') as f:
    entire_file = f.read()
    no_whitespace = "".join(entire_file.strip())
    no_brakets = no_whitespace.translate(None, "[]~()")
    return no_brakets.replace(",", " ")


def chunks(l, n):
    return [l[i:i+n] for i in range(0, len(l), n)]

def breakAt(s, step=2, swap=False):
  words = s.split()
  for t in  chunks(words, step):
    if swap:
      t = t[::-1]
    print " ".join(t)


if __name__ == '__main__':
  clean = format(sys.argv[1])
  if len(sys.argv) > 3:
    if sys.argv[3] =="swap":
      breakAt(clean, int(sys.argv[2]), True)
  else:
    breakAt(clean, int(sys.argv[2]))
