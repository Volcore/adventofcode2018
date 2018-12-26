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

# simple manhattan distance to target, minimum bound on actual distance
def heuristic_cost_estimate(coord,target):
  dx = abs(coord[0] - target[0])
  dy = abs(coord[1] - target[1])
  cost = dx + dy
  if coord[2] != target[2]:
    cost += 7
  return cost

def can_enter_with_tool(tool, region):
  if tool == "torch" and region in [".", "|"]: return True
  if tool == "gear" and region in ["=", "."]: return True
  if tool == "neither" and region in ["|", "="]: return True
  return False

def get_next_coord(open_set, f_score):
  min_c = None
  min_f = 9999999999999
  for coord in open_set:
    f = f_score[coord]
    if f < min_f:
      min_f = f
      min_c = coord
  return min_c

def run_b(ctx):
  # torch = rocky, narrow
  # gear = wet, rocky
  # neither = narrow, wet
  start = (0,0,"torch")
  target = (ctx["target"][0], ctx["target"][1], "torch")
  open_set = set([start])
  closed_set = set()
  g_score = {start:0}
  f_score = {start:heuristic_cost_estimate(start, target)}
  while True:
    # find the cheapest next cell
    coords = get_next_coord(open_set, f_score)
    if coords == target:
      break
    # print("next cell: %s"%(str(coords)))
    # print("gscore: %s"%g_score)
    # remove from todo list
    open_set.remove(coords)
    closed_set.add(coords)
    # fetch values
    current_g_score = g_score[coords]
    r_type = region_type(ctx, (coords[0], coords[1]))
    # go through all neighbors for the cell using the same tool
    for (dx, dy) in [(-1,0),(0,-1),(1,0),(0,1)]:
      # compute coords
      nx = coords[0] + dx
      ny = coords[1] + dy
      nc = (nx, ny, coords[2])
      # check bounds
      if nx < 0 or ny < 0: continue
      # check if we can enter with current tool
      nr_type = region_type(ctx, (nc[0], nc[1]))
      if not can_enter_with_tool(nc[2], nr_type): continue
      if nc in closed_set: continue
      # compute new gscore
      new_g_score = current_g_score + 1
      # if not in open set, add to open set
      if not nc in open_set:
        open_set.add(nc)
      elif new_g_score >= g_score[nc]:
        continue
      g_score[nc] = new_g_score
      f_score[nc] = new_g_score + heuristic_cost_estimate(nc, target)
    # go through all tools for the cell
    for nt in ["torch", "gear", "neither"]:
      # skip if the same
      if coords[2] == nt: continue
      if not can_enter_with_tool(nt, r_type): continue
      # add neighbor to list
      nc = (coords[0], coords[1], nt)
      if nc in closed_set: continue
      # compute new gscore
      new_g_score = current_g_score + 7
      # if not in open set, add to open set
      if not nc in open_set:
        open_set.add(nc)
      elif new_g_score >= g_score[nc]:
        continue
      g_score[nc] = new_g_score
      f_score[nc] = new_g_score + heuristic_cost_estimate(nc, target)
  # return the lowest cost for the target
  tt = g_score[target]
  return tt

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
assert run_a(testInput) == 114

print("Solution for A: %s"%run_a(make_context(5913, (8,701))))

assert run_b(testInput) == 45

print("Solution for B: %s"%run_b(make_context(5913, (8,701))))
