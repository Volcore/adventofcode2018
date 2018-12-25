# Switching to python... sorry. Haskell was too much to handle.

def parse(text):
  return [list(map(int, line.split(","))) for line in text.splitlines()]

def distance(a, b): return sum([abs(a[i]-b[i]) for i in [0,1,2,3]])

def close_to_cluster(cluster, coord):
  for cc in cluster:
    if distance(cc, coord) <= 3:
      return True
  return False

def partition(coords):
  clusterlist = []
  while len(coords) > 0:
    cluster = [coords[0]]
    coords.remove(coords[0])
    found = True
    while found:
      found = False
      for coord in coords:
        if close_to_cluster(cluster, coord):
          cluster.append(coord)
          coords.remove(coord)
          found = True
          break
    clusterlist.append(cluster)
  return clusterlist


def run_a(text):
  coords = parse(text)
  cs = partition(coords)
  return len(cs)

testInput1 = """\
 0,0,0,0
 3,0,0,0
 0,3,0,0
 0,0,3,0
 0,0,0,3
 0,0,0,6
 9,0,0,0
12,0,0,0"""
assert run_a(testInput1) == 2

testInput2 = """\
-1,2,2,0
0,0,2,-2
0,0,0,-2
-1,2,0,0
-2,-2,-2,2
3,0,2,-1
-1,3,2,2
-1,0,-1,0
0,2,1,-2
3,0,0,0"""
assert run_a(testInput2) == 4

testInput3 = """\
1,-1,0,1
2,0,-1,0
3,2,-1,0
0,0,3,1
0,0,-1,-1
2,3,-2,0
-2,2,0,0
2,-2,0,-1
1,-1,0,-1
3,2,0,2"""
assert run_a(testInput3) == 3

testInput4 = """\
1,-1,-1,-2
-2,-2,0,1
0,2,1,3
-2,3,-2,1
0,2,3,-2
-1,-1,1,-2
0,-2,-1,0
-2,2,3,-1
1,2,2,0
-1,-2,0,-2"""
assert run_a(testInput4) == 8

print("Solution for A: %s"%(run_a(open("input.txt", "rt").read())))