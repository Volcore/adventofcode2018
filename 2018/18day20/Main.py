# Switching to python... sorry. Haskell was too much to handle.
import sys
sys.setrecursionlimit(10**6)

def parse(text):
  assert text[0] == '^'
  def subtree(text):
    c = text[0]
    # tree = {}
    if c == '$': return (None, "")
    if c == '|': return (None, text[1:])
    if c == ')': return (None, text)
    if c in "NEWS":
      sub = subtree(text[1:])
      tree = {c:sub[0]}
      return (tree, sub[1])
    if c == '(':
      tree = {}
      rem = text[1:]
      while rem[0] != ')':
        c2 = rem[0]
        rem = rem[1:]
        sub = subtree(rem)
        tree[c2] = sub[0]
        rem = sub[1]
      # need to continue parsing here, there might be more tree
      remtree = subtree(rem[1:])
      if remtree[0]: tree.update(remtree[0])
      return (tree, remtree[1])
    print("unknown text piece %s"%text)
    assert text
    return (None, text)
  tree, rem = subtree(text[1:])
  assert len(rem) == 0
  return tree

def find_longest_subtree(tree):
  if tree == None: return 0
  longest = 0
  for key in tree:
    l = find_longest_subtree(tree[key]) + 1
    if l > longest: longest = l
  return longest

def count_rooms_at_distance(tree, distance):
  if tree == None: return 0
  count = 0
  if distance <= 0: count += len(tree)
  for key in tree:
    count += count_rooms_at_distance(tree[key], distance-1)
  return count

def contract(tree, prev='^'):
  if tree == None: return None
  newtree = {}
  for key in tree:
    # walking back means end of path
    if prev == 'E' and key == 'W': continue
    if prev == 'W' and key == 'E': continue
    if prev == 'N' and key == 'S': continue
    if prev == 'S' and key == 'N': continue
    newtree[key] = contract(tree[key], key)
  return newtree

def run_a(text):
  tree = parse(text)
  tree = contract(tree)
  return find_longest_subtree(tree)

def run_b(text, distance):
  tree = parse(text)
  tree = contract(tree)
  return count_rooms_at_distance(tree, distance - 1)

testInput1 = "^WNE$"
assert run_a(testInput1) == 3
testInput2 = "^ENWWW(NEEE|SSE(EE|N))$"
assert run_a(testInput2) == 10
testInput3 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
assert run_a(testInput3) == 18
testInput4 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
assert run_a(testInput4) == 23
testInput5 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
assert run_a(testInput5) == 31

input = open("input.txt", "rt").read()
print("Solution for A: %s"%run_a(input))

assert run_b(testInput1, 1) == 3
assert run_b(testInput2, 5) == 11

print("Solution for B: %s"%run_b(input, 1000))
