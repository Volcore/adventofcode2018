# Switching to python... sorry. Haskell was too much to handle.
def geo_index(ctx, coord):
  gi = ctx["gi"]
  if coord in gi: return gi[coord]
  if coord in [(0,0), ctx["target"]]: return 0
  if coord[0] == 0: return coord[1] * 48271
  if coord[1] == 0: return coord[0] * 16807
  t = (coord[0], coord[1]-1)
  elt = erosion_level(ctx, t)
  l = (coord[0]-1, coord[1])
  ell = erosion_level(ctx, l)
  gic = elt * ell
  gi[coord] = gic
  return gic

def erosion_level_impl(gi, depth):
  return (gi + depth) % 20183

def erosion_level(ctx, coord):
  return erosion_level_impl(geo_index(ctx, coord), ctx["depth"])

def risk_level_impl(el):
  return el % 3

def region_type_impl(el):
  return ".=|"[risk_level_impl(el)]

def region_type(ctx, coord):
  return region_type_impl(erosion_level(ctx, coord))

def risk_level(ctx, coord):
  return risk_level_impl(erosion_level(ctx, coord))

def total_risk(ctx):
  risk = 0
  for y in range(0, ctx["target"][1]+1):
    for x in range(0, ctx["target"][0]+1):
      risk += risk_level(ctx, (x, y))
  return risk

def run_a(ctx):
  return total_risk(ctx)

def make_context(depth, target):
  return dict(depth=depth, target=target, gi=dict())

def plot(ctx, w, h):
  s = ""
  for y in range(h):
    for x in range(w):
      if (x,y) == (0,0): s += "M"
      elif (x,y) == ctx["target"]: s += "T"
      else: s += str(region_type(ctx, (x,y)))
    s += "\n"
  print(s)

  

testInput = make_context(510, (10,10))
assert geo_index(testInput, (0,0)) == 0
assert erosion_level(testInput, (0,0)) == 510
assert region_type(testInput, (0,0)) == '.'
assert erosion_level(testInput, (1,0)) == 17317
assert region_type(testInput, (1,0)) == '='
assert erosion_level(testInput, (0,1)) == 8415
assert region_type(testInput, (0,1)) == '.'
assert erosion_level(testInput, (1,1)) == 1805
assert region_type(testInput, (1,1)) == '|'
assert erosion_level(testInput, (10,10)) == 510
assert region_type(testInput, (10,10)) == '.'
plot(testInput, 16, 16)
assert run_a(testInput) == 114

print("Solution for A: %s"%run_a(make_context(5913, (8,701))))