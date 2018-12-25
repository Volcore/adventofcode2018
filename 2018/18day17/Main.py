# Switching to python... sorry. Haskell was too much to handle.
def bounds(ctx):
  mins = [999999,999999]
  maxs = [-999999,-999999]
  for w in ctx["m"]:
    mins[0] = min(w[0], mins[0])
    maxs[0] = max(w[0], maxs[0])
    mins[1] = min(w[1], mins[1])
    maxs[1] = max(w[1], maxs[1])
  return mins + maxs

def parse(text):
  walls = []
  for line in text.splitlines():
    l,r = line.split(' ')
    l1 = l[2:-1]
    _,r1 = r.split('=')
    rmin,_,rmax = r1.split('.')
    seq = range(int(rmin), int(rmax)+1)
    if l[0] == 'x':
      walls += [(int(l1),y) for y in seq]
    else:
      walls += [(x,int(l1)) for x in seq]
  m = dict()
  for wall in walls: m[wall] = '#'
  ctx = dict(m=m, flow_queue=[])
  # compute bounds
  ctx["bounds"] = bounds(ctx)
  # insert source
  m[(500,0)] = '+'
  source = (500,0)
  add_flow(ctx, source[0], source[1]+1)
  return ctx

def celltype(ctx, x, y):
  return ctx["m"].get((x,y), '.')

def plot(ctx):
  minmax = ctx["bounds"]
  s = ""
  for y in range(0, minmax[3]+2):
    for x in range(minmax[0]-1, minmax[2]+2):
      s += str(celltype(ctx, x, y))
    s += "\n"
  print(s)

def add_flow(ctx, x, y):
  if ctx["m"].get((x,y), '.') != '.': return
  ctx["m"][(x,y)] = '|'
  ctx["flow_queue"].append((x,y))

def add_water(ctx, x, y):
  old_t = ctx["m"].get((x,y), '.')
  if old_t == '~': return
  ctx["m"][(x,y)] = '~'

def purge_y(ctx, max_y, min_y=-1):
  dellist = []
  for k in ctx["m"].keys():
    if k[1] > max_y: dellist.append(k)
    if min_y >= 0 and k[1] < min_y: dellist.append(k)
  for k in dellist: del ctx["m"][k]

def check_row_water(ctx, x, y):
  def check_side(x1, d):
    x1 += d
    while True:
      t = celltype(ctx, x1, y)
      if t == '#': return x1
      tb = celltype(ctx, x1, y+1)
      if t in '|.' and tb in "#~":
        x1 = x1+d
        continue
      return None
  l = check_side(x, -1)
  if l == None: return False
  r = check_side(x, 1)
  if r == None: return False
  # add water
  for xp in range(l+1, r):
    add_water(ctx,xp,y)
    # reactivate flows above
    ctx["flow_queue"].append((xp, y-1))
  return True

def step(ctx):
  queue = ctx["flow_queue"]
  ctx["flow_queue"] = []
  for flow in queue:
    tf = celltype(ctx, flow[0], flow[1])
    if tf != '|': continue
    t = celltype(ctx, flow[0], flow[1]+1)
    if t == '|': continue
    if t == '.': add_flow(ctx, flow[0], flow[1]+1)
    if t == '~' or t == '#':
      if check_row_water(ctx, flow[0], flow[1]):
        continue
      # move flow on x axis
      xl, xr, y = flow[0]-1, flow[0]+1, flow[1]
      if celltype(ctx, xl, y) == '.': add_flow(ctx, xl, y)
      if celltype(ctx, xr, y) == '.': add_flow(ctx, xr, y)
  # Remove any element that is too low
  minmax = ctx["bounds"]
  purge_y(ctx, minmax[3])

def count(ctx, types="~|"):
  c = 0
  for x in ctx["m"].values():
    if x in types: c += 1
  return c

def simulate(ctx):
  old_count = 0
  while True:
    step(ctx)
    new_count = count(ctx)
    if new_count == old_count:
      return
    print(new_count)
    old_count = new_count 

def run_a(text):
  ctx = parse(text)
  simulate(ctx)
  # remove extra flow
  minmax = ctx["bounds"]
  purge_y(ctx, minmax[3], minmax[1])
  # final count
  counta = count(ctx)
  countb = count(ctx, "~")
  print("A:", counta)
  print("B:", countb)
  plot(ctx)
  return (counta, countb)

testInput1 = """\
x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504"""
assert run_a(testInput1) == (57, 29)

input = open("input.txt", "rt").read()
print("Solution for A: %s\nSolution for B:%s"%run_a(input))